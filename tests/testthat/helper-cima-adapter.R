cima_adapter_env <- new.env(parent = baseenv())

sys.source(
  file.path(project_root(), "R", "version.R"),
  envir = cima_adapter_env
)

for (domain_file in c(
    "medicinal_products.R",
    "excipients.R",
    "source_artifacts.R",
    "evidence.R",
    "verification.R")) {
  sys.source(
    file.path(project_root(), "R", "domain", domain_file),
    envir = cima_adapter_env
  )
}

sys.source(
  file.path(project_root(), "R", "application", "ports.R"),
  envir = cima_adapter_env
)
sys.source(
  file.path(project_root(), "R", "application", "suggestions.R"),
  envir = cima_adapter_env
)

for (adapter_file in c(
    "http_policy.R",
    "cima_client.R",
    "cima_suggestions.R",
    "cima_mapper.R",
    "cima_sources.R",
    "cima_document_client.R",
    "cima_document_mapper.R",
    "cima_document_sources.R")) {
  sys.source(
    file.path(project_root(), "R", "adapters", adapter_file),
    envir = cima_adapter_env
  )
}

cima_fixture_path <- function(name) {
  file.path(project_root(), "tests", "fixtures", "cima-v1.23", name)
}

read_cima_fixture <- function(name) {
  jsonlite::fromJSON(cima_fixture_path(name), simplifyVector = FALSE)
}

# All fixtures are synthetic and derived from the documented CIMA v1.23 shape.
cima_fixture_provenance <- function() {
  "synthetic fixture derived from documented CIMA v1.23 shape"
}

new_recording_cima_transport <- function(handler) {
  calls <- list()
  transport <- function(url, query) {
    calls[[length(calls) + 1L]] <<- list(url = url, query = query)
    handler(url, query)
  }
  list(transport = transport, calls = function() calls)
}

new_recording_cima_document_transport <- function(handler) {
  calls <- list()
  transport <- function(url, query, accept) {
    calls[[length(calls) + 1L]] <<- list(
      url = url,
      query = query,
      accept = accept
    )
    handler(url, query, accept)
  }
  list(transport = transport, calls = function() calls)
}

read_cima_text_fixture <- function(name) {
  paste(
    readLines(cima_fixture_path(name), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

document_transport_response <- function(
    content,
    content_type,
    status_code = 200L) {
  list(
    status_code = status_code,
    content = content,
    content_type = content_type
  )
}
