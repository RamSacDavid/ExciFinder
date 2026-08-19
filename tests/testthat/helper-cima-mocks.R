fixture_text <- function(name) {
  paste(readLines(file.path(fixtures_path, name), warn = FALSE), collapse = "\n")
}

mock_response <- function(body = "{}", status = 200L) {
  structure(
    list(
      status_code = as.integer(status),
      content = charToRaw(body),
      headers = list(),
      url = "mock://cima"
    ),
    class = "response"
  )
}

make_cima_mock <- function(
    medicamentos = "medicamentos-one.json",
    documento = "doc-61-present.json",
    initial_status = 200L,
    document_status = 200L) {
  function(url, query = NULL, ...) {
    if (identical(url, "https://cima.aemps.es/cima/rest/medicamentos")) {
      if (identical(as.integer(initial_status), 200L)) {
        return(mock_response(fixture_text(medicamentos), initial_status))
      }
      return(mock_response(fixture_text("doc-error.json"), initial_status))
    }

    if (startsWith(
      url,
      "https://cima.aemps.es/cima/rest/docSegmentado/contenido/1?nregistro="
    )) {
      if (identical(as.integer(document_status), 200L)) {
        return(mock_response(fixture_text(documento), document_status))
      }
      return(mock_response(fixture_text("doc-error.json"), document_status))
    }

    stop(sprintf("Unexpected network request: %s", url), call. = FALSE)
  }
}
