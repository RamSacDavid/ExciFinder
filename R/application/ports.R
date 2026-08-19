.port_abort <- function(message) {
  stop(message, call. = FALSE)
}

.port_assert_non_empty_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .port_abort(sprintf("`%s` must be a non-empty character string.", name))
  }
  invisible(value)
}

.port_assert_optional_string <- function(value, name) {
  if (!is.null(value)) {
    .port_assert_non_empty_string(value, name)
  }
  invisible(value)
}

.port_assert_function <- function(value, name) {
  if (!is.function(value)) {
    .port_abort(sprintf("`%s` must be a function.", name))
  }
  invisible(value)
}

.new_port <- function(operations, class_name) {
  structure(operations, class = c(class_name, "excifinder_port"))
}

# Adapters return this explicit sentinel when a single requested resource is absent.
# Collection operations return an empty list when their query has no matches.
new_port_absent <- function(reason = "not_found") {
  .port_assert_non_empty_string(reason, "reason")
  structure(list(reason = reason), class = "port_absent")
}

is_port_absent <- function(x) {
  inherits(x, "port_absent")
}

# Adapters signal this condition for operational failures; absence is not an error.
new_port_error <- function(message, port, operation) {
  .port_assert_non_empty_string(message, "message")
  .port_assert_non_empty_string(port, "port")
  .port_assert_non_empty_string(operation, "operation")
  structure(
    list(message = message, call = NULL, port = port, operation = operation),
    class = c("excifinder_port_error", "error", "condition")
  )
}

new_source_content <- function(
    source_artifact_id,
    content,
    content_type,
    section = NULL,
    retrieval_method,
    retrieved_at = NULL,
    content_hash = NULL) {
  .port_assert_non_empty_string(source_artifact_id, "source_artifact_id")
  if (!is.character(content) || length(content) != 1L || is.na(content)) {
    .port_abort("`content` must be a single, non-missing character string.")
  }
  .port_assert_non_empty_string(content_type, "content_type")
  .port_assert_optional_string(section, "section")
  .port_assert_non_empty_string(retrieval_method, "retrieval_method")
  .port_assert_optional_string(content_hash, "content_hash")

  # Successful content retrieval does not by itself establish formulation-level verification coverage.
  structure(
    list(
      source_artifact_id = source_artifact_id,
      content = content,
      content_type = content_type,
      section = section,
      retrieval_method = retrieval_method,
      retrieved_at = retrieved_at,
      content_hash = content_hash
    ),
    class = c("source_content", "excifinder_application_dto")
  )
}

is_source_content <- function(x) {
  inherits(x, "source_content")
}

new_source_excipient_entry <- function(
    source_artifact_id,
    source_record_id = NULL,
    subject_id,
    name,
    quantity = NULL,
    unit = NULL,
    position = NULL) {
  .port_assert_non_empty_string(source_artifact_id, "source_artifact_id")
  .port_assert_optional_string(source_record_id, "source_record_id")
  .port_assert_non_empty_string(subject_id, "subject_id")
  .port_assert_non_empty_string(name, "name")
  .port_assert_optional_string(unit, "unit")

  structure(
    list(
      source_artifact_id = source_artifact_id,
      source_record_id = source_record_id,
      subject_id = subject_id,
      name = name,
      quantity = quantity,
      unit = unit,
      position = position
    ),
    class = c("source_excipient_entry", "excifinder_application_dto")
  )
}

is_source_excipient_entry <- function(x) {
  inherits(x, "source_excipient_entry")
}

new_source_composition_snapshot <- function(source_artifact, entries = list()) {
  if (!inherits(source_artifact, "source_artifact")) {
    .port_abort("`source_artifact` must be a `source_artifact` object.")
  }
  if (!identical(source_artifact$artifact_type, "structured_record")) {
    .port_abort("`source_artifact` must have `artifact_type` equal to `structured_record`.")
  }
  if (!is.list(entries)) {
    .port_abort("`entries` must be a list of `source_excipient_entry` objects.")
  }

  for (entry in entries) {
    if (!is_source_excipient_entry(entry)) {
      .port_abort("Every item in `entries` must be a `source_excipient_entry` object.")
    }
    if (!identical(entry$source_artifact_id, source_artifact$id)) {
      .port_abort("Every entry must reference `source_artifact$id`.")
    }
    if (!identical(entry$subject_id, source_artifact$subject_id)) {
      .port_abort("Every entry must reference `source_artifact$subject_id`.")
    }
  }

  structure(
    list(source_artifact = source_artifact, entries = entries),
    class = c("source_composition_snapshot", "excifinder_application_dto")
  )
}

is_source_composition_snapshot <- function(x) {
  inherits(x, "source_composition_snapshot")
}

# This source port represents the official product source. Its functions return
# canonical domain objects, a list of canonical domain objects, or `port_absent`.
new_product_source_port <- function(
    find_products_by_active_ingredient = NULL,
    get_product = NULL,
    get_formulations = NULL,
    get_presentations = NULL) {
  .port_assert_function(
    find_products_by_active_ingredient,
    "find_products_by_active_ingredient"
  )
  .port_assert_function(get_product, "get_product")
  .port_assert_function(get_formulations, "get_formulations")
  .port_assert_function(get_presentations, "get_presentations")

  .new_port(
    list(
      find_products_by_active_ingredient = find_products_by_active_ingredient,
      get_product = get_product,
      get_formulations = get_formulations,
      get_presentations = get_presentations
    ),
    "product_source_port"
  )
}

is_product_source_port <- function(x) {
  inherits(x, "product_source_port")
}

# This source port represents official provenance metadata and retrieved content.
# Metadata uses SourceArtifact domain objects; content uses the DTO above.
new_source_artifact_port <- function(
    list_source_artifacts = NULL,
    get_source_artifact = NULL,
    get_source_content = NULL) {
  .port_assert_function(list_source_artifacts, "list_source_artifacts")
  .port_assert_function(get_source_artifact, "get_source_artifact")
  .port_assert_function(get_source_content, "get_source_content")

  .new_port(
    list(
      list_source_artifacts = list_source_artifacts,
      get_source_artifact = get_source_artifact,
      get_source_content = function(source_artifact_id, section = NULL) {
        .port_assert_non_empty_string(source_artifact_id, "source_artifact_id")
        .port_assert_optional_string(section, "section")
        get_source_content(source_artifact_id, section)
      }
    ),
    "source_artifact_port"
  )
}

is_source_artifact_port <- function(x) {
  inherits(x, "source_artifact_port")
}

# This port exposes source-native structured composition and its provenance
# atomically, without resolving entries to canonical concepts or applying matching rules.
new_composition_source_port <- function(get_composition_snapshot = NULL) {
  .port_assert_function(get_composition_snapshot, "get_composition_snapshot")

  .new_port(
    list(get_composition_snapshot = function(subject_id) {
      .port_assert_non_empty_string(subject_id, "subject_id")
      result <- get_composition_snapshot(subject_id)
      if (!is_source_composition_snapshot(result) && !is_port_absent(result)) {
        .port_abort(
          "`get_composition_snapshot` must return a `source_composition_snapshot` or `port_absent`."
        )
      }
      result
    }),
    "composition_source_port"
  )
}

is_composition_source_port <- function(x) {
  inherits(x, "composition_source_port")
}

# This repository port persists the canonical product catalogue already known
# by the application. It is separate from document and assessment persistence.
new_catalog_repository_port <- function(
    get_product = NULL,
    put_product = NULL,
    get_formulation = NULL,
    put_formulation = NULL,
    get_presentation = NULL,
    put_presentation = NULL) {
  operations <- list(
    get_product = get_product,
    put_product = put_product,
    get_formulation = get_formulation,
    put_formulation = put_formulation,
    get_presentation = get_presentation,
    put_presentation = put_presentation
  )

  for (operation_name in names(operations)) {
    .port_assert_function(operations[[operation_name]], operation_name)
  }

  .new_port(operations, "catalog_repository_port")
}

is_catalog_repository_port <- function(x) {
  inherits(x, "catalog_repository_port")
}

# This repository port persists source-artifact metadata and retrieved content.
new_artifact_repository_port <- function(
    get_artifact = NULL,
    put_artifact = NULL,
    get_source_content = NULL,
    put_source_content = NULL) {
  operations <- list(
    get_artifact = get_artifact,
    put_artifact = put_artifact,
    get_source_content = get_source_content,
    put_source_content = put_source_content
  )

  for (operation_name in names(operations)) {
    .port_assert_function(operations[[operation_name]], operation_name)
  }

  operations$get_source_content <- function(source_artifact_id, section = NULL) {
    .port_assert_non_empty_string(source_artifact_id, "source_artifact_id")
    .port_assert_optional_string(section, "section")
    get_source_content(source_artifact_id, section)
  }
  operations$put_source_content <- function(source_content) {
    if (!is_source_content(source_content)) {
      .port_abort("`source_content` must be a `source_content` object.")
    }
    put_source_content(source_content)
  }

  .new_port(operations, "artifact_repository_port")
}

is_artifact_repository_port <- function(x) {
  inherits(x, "artifact_repository_port")
}

# This repository port persists ExcipientAssessment objects. Assessment keys are
# opaque application values; this boundary validates only that they are present.
new_assessment_repository_port <- function(
    get_assessment = NULL,
    put_assessment = NULL) {
  .port_assert_function(get_assessment, "get_assessment")
  .port_assert_function(put_assessment, "put_assessment")

  .new_port(
    list(
      get_assessment = function(key) {
        .port_assert_non_empty_string(key, "key")
        get_assessment(key)
      },
      put_assessment = function(key, assessment) {
        .port_assert_non_empty_string(key, "key")
        put_assessment(key, assessment)
      }
    ),
    "assessment_repository_port"
  )
}

is_assessment_repository_port <- function(x) {
  inherits(x, "assessment_repository_port")
}
