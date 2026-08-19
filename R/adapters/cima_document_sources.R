.cima_document_sources_abort <- function(message, operation, parent = NULL) {
  condition <- new_port_error(message, "cima_document_sources", operation)
  condition$parent <- parent
  stop(condition)
}

.cima_document_registration_from_product_id <- function(product_id) {
  if (!is.character(product_id) || length(product_id) != 1L || is.na(product_id) ||
      !grepl("^AEMPS:[^:]+$", product_id)) {
    .cima_document_sources_abort(
      "Product ID is outside the AEMPS namespace.",
      "product_id"
    )
  }
  substring(product_id, nchar("AEMPS:") + 1L)
}

.cima_document_product_id_from_artifact_id <- function(source_artifact_id) {
  if (!is.character(source_artifact_id) || length(source_artifact_id) != 1L ||
      is.na(source_artifact_id)) {
    .cima_document_sources_abort("Invalid CIMA document artifact ID.", "artifact_id")
  }
  match <- regexec(
    "^AEMPS:CIMA:medicine:([^:]+):document:",
    source_artifact_id
  )
  parts <- regmatches(source_artifact_id, match)[[1]]
  if (length(parts) != 2L) {
    .cima_document_sources_abort(
      "Artifact ID is outside the CIMA document namespace.",
      "artifact_id"
    )
  }
  paste0("AEMPS:", parts[[2]])
}

.cima_document_raw_documents <- function(raw_medicine) {
  if (is.null(raw_medicine$docs)) {
    return(list())
  }
  if (!is.list(raw_medicine$docs)) {
    .cima_document_sources_abort("CIMA field `docs` must be a list.", "metadata")
  }
  unname(raw_medicine$docs)
}

.cima_document_detail <- function(client, product_id) {
  registration_number <- .cima_document_registration_from_product_id(product_id)
  raw <- tryCatch(
    client$get_medicine(registration_number),
    error = function(error) {
      .cima_document_sources_abort(
        sprintf("CIMA metadata retrieval failed: %s", conditionMessage(error)),
        "metadata",
        parent = error
      )
    }
  )
  if (length(raw) == 0L) new_port_absent() else raw
}

.cima_document_find <- function(raw_medicine, product_id, artifact_id, retrieved_at) {
  raw_documents <- .cima_document_raw_documents(raw_medicine)
  artifacts <- map_cima_document_source_artifacts(
    raw_medicine,
    product_id,
    retrieved_at
  )
  matches <- which(vapply(
    artifacts,
    function(artifact) identical(artifact$id, artifact_id),
    logical(1)
  ))
  if (length(matches) == 0L) {
    return(new_port_absent())
  }
  if (length(matches) != 1L) {
    .cima_document_sources_abort(
      "CIMA metadata contains duplicate document artifact identities.",
      "metadata"
    )
  }
  list(artifact = artifacts[[matches]], raw_document = raw_documents[[matches]])
}

.cima_document_is_segmented <- function(raw_document) {
  value <- raw_document$secc
  !is.null(value) && length(value) == 1L && isTRUE(value[[1]])
}

.cima_document_json_materialize <- function(content, section) {
  payload <- tryCatch(
    jsonlite::fromJSON(content, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (is.null(payload) || !is.list(payload)) {
    return(list(status = "unusable"))
  }
  if (!is.null(payload$seccion) || !is.null(payload$contenido)) {
    payload <- list(payload)
  }
  entries <- unname(payload)
  valid_entries <- Filter(function(entry) {
    is.list(entry) && is.character(entry$contenido) &&
      length(entry$contenido) == 1L && !is.na(entry$contenido)
  }, entries)
  if (length(valid_entries) == 0L) {
    return(list(status = "unusable"))
  }
  if (!is.null(section)) {
    valid_entries <- Filter(function(entry) {
      is.character(entry$seccion) && length(entry$seccion) == 1L &&
        !is.na(entry$seccion) && identical(entry$seccion, section)
    }, valid_entries)
    if (length(valid_entries) == 0L) {
      return(list(status = "not_found"))
    }
  }
  materialized <- paste(
    vapply(valid_entries, function(entry) entry$contenido, character(1)),
    collapse = "\n"
  )
  if (!nzchar(materialized)) {
    return(list(status = "unusable"))
  }
  looks_html <- grepl("<[^>]+>", materialized)
  list(
    status = "success",
    content = materialized,
    content_type = if (looks_html) "text/html" else "text/plain"
  )
}

.cima_document_materialize <- function(response, accept, section) {
  if (identical(accept, "application/json")) {
    return(.cima_document_json_materialize(response$content, section))
  }
  list(
    status = "success",
    content = response$content,
    content_type = accept
  )
}

.cima_document_retrieve_content <- function(
    document_client,
    registration_number,
    artifact,
    section,
    retrieved_at) {
  methods <- c(
    `text/plain` = "cima_segmented_plain",
    `application/json` = "cima_segmented_json",
    `text/html` = "cima_segmented_html"
  )
  for (accept in names(methods)) {
    response <- tryCatch(
      document_client$get_segmented_content(
        registration_number,
        artifact$artifact_kind,
        section = section,
        accept = accept
      ),
      error = function(error) {
        .cima_document_sources_abort(
          sprintf("CIMA segmented content retrieval failed: %s", conditionMessage(error)),
          "get_source_content",
          parent = error
        )
      }
    )
    if (is_cima_document_not_found(response)) {
      return(new_port_absent())
    }
    if (is_cima_document_unusable(response)) {
      next
    }
    materialized <- .cima_document_materialize(response, accept, section)
    if (identical(materialized$status, "not_found")) {
      return(new_port_absent())
    }
    if (identical(materialized$status, "unusable")) {
      next
    }
    return(new_source_content(
      source_artifact_id = artifact$id,
      content = materialized$content,
      content_type = materialized$content_type,
      section = section,
      retrieval_method = unname(methods[[accept]]),
      retrieved_at = retrieved_at
    ))
  }
  .cima_document_sources_abort(
    "CIMA returned no usable segmented representation.",
    "get_source_content"
  )
}

new_cima_document_source_artifact_port <- function(
    client,
    document_client,
    retrieved_at = function() Sys.time()) {
  if (!is_cima_client(client)) {
    .cima_document_sources_abort("`client` must be a CIMA client.", "construction")
  }
  if (!is_cima_document_client(document_client)) {
    .cima_document_sources_abort(
      "`document_client` must be a CIMA document client.",
      "construction"
    )
  }
  if (!is.function(retrieved_at)) {
    .cima_document_sources_abort(
      "`retrieved_at` must be a function.",
      "construction"
    )
  }

  new_source_artifact_port(
    list_source_artifacts = function(subject_id, artifact_type = NULL) {
      operation_time <- retrieved_at()
      raw <- .cima_document_detail(client, subject_id)
      if (is_port_absent(raw)) {
        return(list())
      }
      if (!is.null(artifact_type) && !identical(artifact_type, "document")) {
        return(list())
      }
      map_cima_document_source_artifacts(raw, subject_id, operation_time)
    },
    get_source_artifact = function(source_artifact_id) {
      operation_time <- retrieved_at()
      product_id <- .cima_document_product_id_from_artifact_id(source_artifact_id)
      raw <- .cima_document_detail(client, product_id)
      if (is_port_absent(raw)) {
        return(raw)
      }
      found <- .cima_document_find(
        raw,
        product_id,
        source_artifact_id,
        operation_time
      )
      if (is_port_absent(found)) found else found$artifact
    },
    get_source_content = function(source_artifact_id, section = NULL) {
      # Successful retrieval of SmPC section 6.1 does not by itself establish
      # formulation-level verification coverage.
      operation_time <- retrieved_at()
      product_id <- .cima_document_product_id_from_artifact_id(source_artifact_id)
      registration_number <- .cima_document_registration_from_product_id(product_id)
      raw <- .cima_document_detail(client, product_id)
      if (is_port_absent(raw)) {
        return(raw)
      }
      found <- .cima_document_find(
        raw,
        product_id,
        source_artifact_id,
        operation_time
      )
      if (is_port_absent(found)) {
        return(found)
      }
      artifact <- found$artifact
      if (!artifact$artifact_kind %in% c(
          "summary_of_product_characteristics",
          "package_leaflet")) {
        .cima_document_sources_abort(
          sprintf(
            "Content materialization is unsupported for document kind `%s`.",
            artifact$artifact_kind
          ),
          "get_source_content"
        )
      }
      if (!.cima_document_is_segmented(found$raw_document)) {
        .cima_document_sources_abort(
          "The document exists but segmented content is unavailable.",
          "get_source_content"
        )
      }
      .cima_document_retrieve_content(
        document_client,
        registration_number,
        artifact,
        section,
        operation_time
      )
    }
  )
}
