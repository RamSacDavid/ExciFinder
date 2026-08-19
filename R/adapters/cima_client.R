.cima_client_abort <- function(message, operation, parent = NULL) {
  stop(structure(
    list(
      message = message,
      call = NULL,
      operation = operation,
      parent = parent
    ),
    class = c("cima_client_error", "error", "condition")
  ))
}

.cima_assert_non_empty_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .cima_client_abort(
      sprintf("`%s` must be a non-empty character string.", name),
      "validation"
    )
  }
  invisible(value)
}

.cima_assert_optional_logical <- function(value, name) {
  if (!is.null(value) &&
      (!is.logical(value) || length(value) != 1L || is.na(value))) {
    .cima_client_abort(
      sprintf("`%s` must be NULL or a single logical value.", name),
      "validation"
    )
  }
  invisible(value)
}

.cima_assert_positive_integer <- function(value, name) {
  if (length(value) != 1L || is.na(value) || !is.numeric(value) ||
      value < 1 || value != as.integer(value)) {
    .cima_client_abort(
      sprintf("`%s` must be a positive integer.", name),
      "validation"
    )
  }
  as.integer(value)
}

.cima_default_transport <- function(url, query) {
  response <- excifinder_http_get(url, query = query)
  httr::stop_for_status(response)
  payload <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(payload, simplifyVector = FALSE)
}

.cima_request <- function(transport, url, query, operation) {
  tryCatch(
    {
      response <- transport(url, query)
      if (!is.list(response)) {
        .cima_client_abort(
          "Transport returned an invalid response; an R list was expected.",
          operation
        )
      }
      response
    },
    error = function(error) {
      if (inherits(error, "cima_client_error")) {
        stop(error)
      }
      .cima_client_abort(
        sprintf("CIMA operation `%s` failed: %s", operation, conditionMessage(error)),
        operation,
        parent = error
      )
    }
  )
}

.cima_search_query <- function(
    active_ingredient,
    authorized = NULL,
    marketed = NULL,
    page = 1L) {
  .cima_assert_non_empty_string(active_ingredient, "active_ingredient")
  .cima_assert_optional_logical(authorized, "authorized")
  .cima_assert_optional_logical(marketed, "marketed")
  page <- .cima_assert_positive_integer(page, "page")

  query <- list(practiv1 = active_ingredient, pagina = page)
  if (!is.null(authorized)) {
    query$autorizados <- as.integer(authorized)
  }
  if (!is.null(marketed)) {
    query$comerc <- as.integer(marketed)
  }
  query
}

.cima_page_results <- function(page) {
  if (is.null(page$resultados) || !is.list(page$resultados)) {
    .cima_client_abort(
      "Paged response must contain a `resultados` list.",
      "find_medicines_page"
    )
  }
  unname(page$resultados)
}

.cima_optional_count <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value) ||
      !is.numeric(value) || value < 0 || value != as.integer(value)) {
    return(NULL)
  }
  as.integer(value)
}

new_cima_client <- function(
    transport = .cima_default_transport,
    base_url = "https://cima.aemps.es/cima/rest") {
  if (!is.function(transport)) {
    .cima_client_abort("`transport` must be a function.", "validation")
  }
  .cima_assert_non_empty_string(base_url, "base_url")
  base_url <- sub("/+$", "", base_url)

  find_medicines_page <- function(
      active_ingredient,
      authorized = NULL,
      marketed = NULL,
      page = 1L) {
    query <- .cima_search_query(
      active_ingredient = active_ingredient,
      authorized = authorized,
      marketed = marketed,
      page = page
    )
    response <- .cima_request(
      transport,
      paste0(base_url, "/medicamentos"),
      query,
      "find_medicines_page"
    )
    .cima_page_results(response)
    response
  }

  find_all_medicines <- function(
      active_ingredient,
      authorized = NULL,
      marketed = NULL,
      max_pages = 1000L) {
    max_pages <- .cima_assert_positive_integer(max_pages, "max_pages")
    medicines <- list()

    for (page_number in seq_len(max_pages)) {
      page <- find_medicines_page(
        active_ingredient = active_ingredient,
        authorized = authorized,
        marketed = marketed,
        page = page_number
      )
      page_results <- .cima_page_results(page)
      medicines <- c(medicines, page_results)

      total_rows <- .cima_optional_count(page$totalFilas)
      page_size <- .cima_optional_count(page$tamanioPagina)
      if (length(page_results) == 0L ||
          (!is.null(total_rows) && length(medicines) >= total_rows) ||
          (!is.null(page_size) && length(page_results) < page_size)) {
        return(unname(medicines))
      }
    }

    .cima_client_abort(
      sprintf("Pagination exceeded the safety limit of %d pages.", max_pages),
      "find_all_medicines"
    )
  }

  get_medicine <- function(registration_number) {
    .cima_assert_non_empty_string(registration_number, "registration_number")
    .cima_request(
      transport,
      paste0(base_url, "/medicamento"),
      list(nregistro = registration_number),
      "get_medicine"
    )
  }

  structure(
    list(
      find_medicines_page = find_medicines_page,
      find_all_medicines = find_all_medicines,
      get_medicine = get_medicine
    ),
    class = "cima_client"
  )
}

is_cima_client <- function(x) {
  inherits(x, "cima_client")
}
