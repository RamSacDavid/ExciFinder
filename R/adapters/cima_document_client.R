.cima_document_client_abort <- function(message, operation, parent = NULL) {
  stop(structure(
    list(
      message = message,
      call = NULL,
      operation = operation,
      parent = parent
    ),
    class = c("cima_document_client_error", "error", "condition")
  ))
}

.cima_document_assert_non_empty_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .cima_document_client_abort(
      sprintf("`%s` must be a non-empty character string.", name),
      "validation"
    )
  }
  invisible(value)
}

.cima_document_assert_optional_string <- function(value, name) {
  if (!is.null(value)) {
    .cima_document_assert_non_empty_string(value, name)
  }
  invisible(value)
}

.cima_document_default_transport <- function(url, query, accept) {
  response <- httr::GET(
    url,
    query = query,
    httr::add_headers(Accept = accept)
  )
  content_type <- httr::headers(response)[["content-type"]]
  content <- rawToChar(httr::content(response, as = "raw"))
  list(
    status_code = httr::status_code(response),
    content = content,
    content_type = content_type
  )
}

.cima_document_media_type <- function(content_type) {
  if (!is.character(content_type) || length(content_type) != 1L ||
      is.na(content_type) || !nzchar(trimws(content_type))) {
    return(NULL)
  }
  tolower(trimws(strsplit(content_type, ";", fixed = TRUE)[[1]][[1]]))
}

.cima_segmented_document_type <- function(document_type) {
  types <- c(
    summary_of_product_characteristics = 1L,
    package_leaflet = 2L
  )
  .cima_document_assert_non_empty_string(document_type, "document_type")
  if (!document_type %in% names(types)) {
    .cima_document_client_abort(
      sprintf("Segmented content is unsupported for document type `%s`.", document_type),
      "validation"
    )
  }
  unname(types[[document_type]])
}

new_cima_document_not_found <- function() {
  structure(list(reason = "not_found"), class = "cima_document_not_found")
}

is_cima_document_not_found <- function(x) {
  inherits(x, "cima_document_not_found")
}

new_cima_document_unusable <- function(reason) {
  structure(list(reason = reason), class = "cima_document_unusable")
}

is_cima_document_unusable <- function(x) {
  inherits(x, "cima_document_unusable")
}

new_cima_document_client <- function(
    transport = .cima_document_default_transport,
    base_url = "https://cima.aemps.es/cima/rest") {
  if (!is.function(transport)) {
    .cima_document_client_abort("`transport` must be a function.", "validation")
  }
  .cima_document_assert_non_empty_string(base_url, "base_url")
  base_url <- sub("/+$", "", base_url)

  get_segmented_content <- function(
      registration_number,
      document_type,
      section = NULL,
      accept = "text/plain") {
    .cima_document_assert_non_empty_string(
      registration_number,
      "registration_number"
    )
    .cima_document_assert_optional_string(section, "section")
    .cima_document_assert_non_empty_string(accept, "accept")
    native_document_type <- .cima_segmented_document_type(document_type)
    query <- list(nregistro = registration_number)
    if (!is.null(section)) {
      query$seccion <- section
    }

    response <- tryCatch(
      transport(
        paste0(base_url, "/docSegmentado/contenido/", native_document_type),
        query,
        accept
      ),
      error = function(error) {
        .cima_document_client_abort(
          sprintf("CIMA document transport failed: %s", conditionMessage(error)),
          "get_segmented_content",
          parent = error
        )
      }
    )
    if (!is.list(response) || length(response$status_code) != 1L ||
        is.na(response$status_code) || !is.numeric(response$status_code)) {
      .cima_document_client_abort(
        "Document transport returned an invalid response.",
        "get_segmented_content"
      )
    }

    status_code <- as.integer(response$status_code)
    if (identical(status_code, 404L)) {
      return(new_cima_document_not_found())
    }
    if (status_code < 200L || status_code >= 300L) {
      .cima_document_client_abort(
        sprintf("CIMA document request failed with HTTP status %d.", status_code),
        "get_segmented_content"
      )
    }

    content_type <- .cima_document_media_type(response$content_type)
    expected_type <- .cima_document_media_type(accept)
    content <- response$content
    if (is.raw(content)) {
      content <- rawToChar(content)
    }
    if (!is.character(content) || length(content) != 1L || is.na(content) ||
        !nzchar(content)) {
      return(new_cima_document_unusable("empty_content"))
    }
    if (is.null(content_type) || !identical(content_type, expected_type)) {
      return(new_cima_document_unusable("unexpected_media_type"))
    }

    structure(
      list(
        content = content,
        content_type = content_type,
        status_code = status_code
      ),
      class = "cima_document_response"
    )
  }

  structure(
    list(get_segmented_content = get_segmented_content),
    class = "cima_document_client"
  )
}

is_cima_document_client <- function(x) {
  inherits(x, "cima_document_client")
}
