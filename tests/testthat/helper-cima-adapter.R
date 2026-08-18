cima_adapter_env <- new.env(parent = baseenv())

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

for (adapter_file in c("cima_client.R", "cima_mapper.R", "cima_sources.R")) {
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
