.cima_document_mapper_abort <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("cima_document_mapper_error", "error", "condition")
  ))
}

.cima_document_raw_present <- function(value) {
  !is.null(value) && length(value) > 0L && !all(is.na(value))
}

.cima_document_required_text <- function(value, name) {
  if (!.cima_document_raw_present(value)) {
    .cima_document_mapper_abort(sprintf("Raw field `%s` is required.", name))
  }
  value <- as.character(value[[1]])
  if (!nzchar(trimws(value))) {
    .cima_document_mapper_abort(sprintf("Raw field `%s` cannot be empty.", name))
  }
  value
}

.cima_document_optional_text <- function(value) {
  if (!.cima_document_raw_present(value)) {
    return(NULL)
  }
  value <- as.character(value[[1]])
  if (nzchar(trimws(value))) value else NULL
}

.cima_document_type <- function(raw_type) {
  if (!.cima_document_raw_present(raw_type)) {
    .cima_document_mapper_abort("Raw document field `tipo` is required.")
  }
  raw_type <- suppressWarnings(as.integer(raw_type[[1]]))
  kinds <- c(
    `1` = "summary_of_product_characteristics",
    `2` = "package_leaflet",
    `3` = "public_assessment_report",
    `4` = "risk_management_plan"
  )
  key <- as.character(raw_type)
  if (is.na(raw_type) || !key %in% names(kinds)) {
    .cima_document_mapper_abort(
      sprintf("Unknown CIMA document type `%s`.", as.character(raw_type))
    )
  }
  unname(kinds[[key]])
}

.cima_document_source_date <- function(raw_date) {
  if (!.cima_document_raw_present(raw_date)) {
    return(NULL)
  }
  raw_date <- suppressWarnings(as.numeric(raw_date[[1]]))
  if (is.na(raw_date) || !is.finite(raw_date)) {
    .cima_document_mapper_abort("Raw document field `fecha` must be epoch milliseconds.")
  }
  as.POSIXct(raw_date / 1000, origin = "1970-01-01", tz = "UTC")
}

.cima_document_version_token <- function(source_date) {
  if (is.null(source_date)) {
    return("undated")
  }
  format(source_date, "%Y%m%dT%H%M%SZ", tz = "UTC", usetz = FALSE)
}

.cima_document_stable_token <- function(value) {
  if (is.null(value)) {
    return("no-url")
  }
  hash <- 0
  for (code in utf8ToInt(enc2utf8(value))) {
    hash <- (hash * 131 + code) %% 2147483647
  }
  sprintf("%08x", as.integer(hash))
}

.cima_document_product_registration <- function(medicinal_product_id) {
  if (!is.character(medicinal_product_id) || length(medicinal_product_id) != 1L ||
      is.na(medicinal_product_id) ||
      !grepl("^AEMPS:[^:]+$", medicinal_product_id)) {
    .cima_document_mapper_abort(
      "`medicinal_product_id` must belong to the AEMPS product namespace."
    )
  }
  substring(medicinal_product_id, nchar("AEMPS:") + 1L)
}

cima_document_artifact_id <- function(
    registration_number,
    artifact_kind,
    source_date,
    url) {
  paste(
    "AEMPS:CIMA:medicine",
    registration_number,
    "document",
    artifact_kind,
    .cima_document_version_token(source_date),
    .cima_document_stable_token(url),
    sep = ":"
  )
}

map_cima_document_source_artifact <- function(
    raw_document,
    medicinal_product_id,
    retrieved_at) {
  if (!is.list(raw_document)) {
    .cima_document_mapper_abort("Raw document must be a list.")
  }
  registration_number <- .cima_document_product_registration(medicinal_product_id)
  artifact_kind <- .cima_document_type(raw_document$tipo)
  source_date <- .cima_document_source_date(raw_document$fecha)
  url <- .cima_document_optional_text(raw_document$url)

  new_source_artifact(
    id = cima_document_artifact_id(
      registration_number,
      artifact_kind,
      source_date,
      url
    ),
    source = "AEMPS:CIMA",
    subject_id = medicinal_product_id,
    artifact_type = "document",
    artifact_kind = artifact_kind,
    url = url,
    source_date = source_date,
    retrieved_at = retrieved_at,
    version = .cima_document_version_token(source_date),
    language = "es"
  )
}

map_cima_document_source_artifacts <- function(
    raw_medicine,
    medicinal_product_id,
    retrieved_at) {
  raw_documents <- raw_medicine$docs
  if (is.null(raw_documents)) {
    return(list())
  }
  if (!is.list(raw_documents)) {
    .cima_document_mapper_abort("Raw medicine field `docs` must be a list.")
  }
  lapply(
    unname(raw_documents),
    map_cima_document_source_artifact,
    medicinal_product_id = medicinal_product_id,
    retrieved_at = retrieved_at
  )
}
