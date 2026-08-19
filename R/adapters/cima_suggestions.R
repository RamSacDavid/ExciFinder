.cima_suggestion_default_transport <- function(url, query) {
  response <- excifinder_http_get(url, query = query)
  status <- httr::status_code(response)
  if (identical(status, 404L)) {
    return(list(status = status, payload = list(resultados = list())))
  }
  httr::stop_for_status(response)
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  list(
    status = status,
    payload = jsonlite::fromJSON(text, simplifyVector = FALSE)
  )
}

.cima_suggestion_payload <- function(response) {
  if (!is.list(response)) {
    stop("CIMA suggestion transport returned an invalid response.", call. = FALSE)
  }
  if (!is.null(response$status)) {
    if (identical(as.integer(response$status), 404L)) return(list())
    if (as.integer(response$status) < 200L || as.integer(response$status) >= 300L) {
      stop("CIMA suggestion request failed.", call. = FALSE)
    }
    response <- response$payload
  }
  if (is.null(response)) return(list())
  if (!is.null(response$resultados)) response <- response$resultados
  if (!is.list(response)) list() else unname(response)
}

new_cima_active_ingredient_suggestion_source <- function(
    transport = .cima_suggestion_default_transport,
    base_url = "https://cima.aemps.es/cima/rest") {
  if (!is.function(transport)) {
    stop("`transport` must be a function.", call. = FALSE)
  }
  .cima_assert_non_empty_string(base_url, "base_url")
  base_url <- sub("/+$", "", base_url)

  new_suggestion_source_port(function(query, limit) {
    rows <- tryCatch(
      .cima_suggestion_payload(transport(
        paste0(base_url, "/maestras"),
        list(maestra = 1L, nombre = query)
      )),
      error = function(error) stop(error)
    )
    suggestions <- list()
    for (row in rows) {
      if (!is.list(row)) next
      id <- row$id
      if (is.null(id)) id <- row$codigo
      label <- row$nombre
      if (is.null(id) || is.null(label)) next
      id <- as.character(id)
      label <- as.character(label)
      if (length(id) != 1L || is.na(id) || !nzchar(trimws(id)) ||
          length(label) != 1L || is.na(label) || !nzchar(trimws(label))) next
      suggestions[[length(suggestions) + 1L]] <- new_suggestion(id, label, label)
      if (length(suggestions) >= limit) break
    }
    suggestions
  })
}
