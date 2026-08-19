.excipient_evidence_encode_field <- function(value) {
  value <- enc2utf8(as.character(value))
  paste0(nchar(value, type = "bytes"), ":", value)
}

.excipient_evidence_location_key <- function(location) {
  if (is.null(location)) {
    return("none")
  }
  values <- vapply(location, function(value) {
    paste(as.character(value), collapse = ",")
  }, character(1))
  paste(
    vapply(
      paste0(names(location), "=", values),
      .excipient_evidence_encode_field,
      character(1)
    ),
    collapse = ""
  )
}

excipient_evidence_id <- function(
    source_artifact_id,
    excipient_id,
    candidate,
    ordinal,
    section = NULL) {
  .excipient_assert_non_empty_string(source_artifact_id, "source_artifact_id")
  .excipient_assert_non_empty_string(excipient_id, "excipient_id")
  if (!is_excipient_match_candidate(candidate)) {
    .excipient_application_abort(
      "`candidate` must be an `excipient_match_candidate` object."
    )
  }
  if (!is.numeric(ordinal) || length(ordinal) != 1L || is.na(ordinal) ||
      ordinal < 1 || ordinal != as.integer(ordinal)) {
    .excipient_application_abort("`ordinal` must be a positive integer.")
  }
  fields <- c(
    source_artifact_id,
    excipient_id,
    if (is.null(section)) "<none>" else section,
    candidate$matched_term,
    .excipient_evidence_location_key(candidate$location),
    as.character(as.integer(ordinal))
  )
  key <- paste(
    vapply(fields, .excipient_evidence_encode_field, character(1)),
    collapse = ""
  )
  token <- paste(sprintf("%02x", as.integer(charToRaw(key))), collapse = "")
  paste0("excifinder:evidence:v1:", token)
}

build_excipient_evidence <- function(
    candidate,
    source_artifact_id,
    subject_id,
    section = NULL,
    ordinal = 1L) {
  if (!is_excipient_match_candidate(candidate)) {
    .excipient_application_abort(
      "`candidate` must be an `excipient_match_candidate` object."
    )
  }
  .excipient_assert_non_empty_string(subject_id, "subject_id")
  new_excipient_evidence(
    id = excipient_evidence_id(
      source_artifact_id,
      candidate$excipient_id,
      candidate,
      ordinal,
      section
    ),
    excipient_id = candidate$excipient_id,
    subject_id = subject_id,
    source_artifact_id = source_artifact_id,
    matched_term = candidate$matched_term,
    section = section,
    excerpt = candidate$excerpt,
    method = candidate$method,
    location = candidate$location
  )
}

.build_evidence_collection <- function(
    candidates,
    source_artifact_id,
    subject_id,
    section = NULL) {
  lapply(seq_along(candidates), function(index) {
    build_excipient_evidence(
      candidates[[index]],
      source_artifact_id,
      subject_id,
      section = section,
      ordinal = index
    )
  })
}

build_structured_excipient_attempt <- function(snapshot, excipient, subject_id) {
  if (!is_source_composition_snapshot(snapshot)) {
    .excipient_application_abort(
      "`snapshot` must be a `source_composition_snapshot` object."
    )
  }
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  .excipient_assert_non_empty_string(subject_id, "subject_id")

  # Structured entries may belong to a child formulation artifact while this
  # first factual policy aggregates positive evidence to MedicinalProduct level.
  candidates <- unlist(
    lapply(snapshot$entries, function(entry) {
      match_excipient_entry(entry, excipient)$matches
    }),
    recursive = FALSE,
    use.names = FALSE
  )
  evidence <- .build_evidence_collection(
    candidates,
    snapshot$source_artifact$id,
    subject_id,
    section = NULL
  )
  new_verification_attempt(
    source = snapshot$source_artifact$source,
    source_artifact_id = snapshot$source_artifact$id,
    method = "structured_composition_controlled_terms",
    section = NULL,
    retrieved_at = snapshot$source_artifact$retrieved_at,
    outcome = if (length(evidence) > 0L) "evidence_found" else "no_evidence",
    extraction_status = "complete",
    evidence = evidence
  )
}

.smpc_61_is_valid <- function(artifact, content) {
  inherits(artifact, "source_artifact") &&
    identical(artifact$artifact_type, "document") &&
    identical(artifact$artifact_kind, "summary_of_product_characteristics") &&
    is_source_content(content) &&
    identical(content$source_artifact_id, artifact$id) &&
    identical(content$section, "6.1") &&
    identical(content$content_type, "text/plain") &&
    nzchar(trimws(content$content))
}

build_smpc_61_excipient_attempt <- function(
    artifact,
    content,
    excipient,
    subject_id) {
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  .excipient_assert_non_empty_string(subject_id, "subject_id")
  if (!.smpc_61_is_valid(artifact, content)) {
    artifact_id <- if (inherits(artifact, "source_artifact")) artifact$id else NULL
    source <- if (inherits(artifact, "source_artifact")) artifact$source else "AEMPS:CIMA"
    return(new_verification_attempt(
      source = source,
      source_artifact_id = artifact_id,
      method = if (is_source_content(content)) content$retrieval_method else NULL,
      section = "6.1",
      retrieved_at = if (is_source_content(content)) content$retrieved_at else NULL,
      outcome = "inconclusive",
      extraction_status = "partial"
    ))
  }

  match_result <- match_excipient_content(content, excipient)
  if (identical(match_result$status, "unsupported_content")) {
    return(new_verification_attempt(
      source = artifact$source,
      source_artifact_id = artifact$id,
      method = content$retrieval_method,
      section = "6.1",
      retrieved_at = content$retrieved_at,
      outcome = "inconclusive",
      extraction_status = "partial"
    ))
  }
  evidence <- .build_evidence_collection(
    match_result$matches,
    artifact$id,
    subject_id,
    section = "6.1"
  )
  new_verification_attempt(
    source = artifact$source,
    source_artifact_id = artifact$id,
    method = content$retrieval_method,
    section = "6.1",
    retrieved_at = content$retrieved_at,
    outcome = if (length(evidence) > 0L) "evidence_found" else "no_evidence",
    extraction_status = "complete",
    evidence = evidence
  )
}

build_unavailable_excipient_attempt <- function(
    source,
    method,
    section = NULL,
    error = NULL) {
  new_verification_attempt(
    source = source,
    method = method,
    section = section,
    outcome = if (is.null(error)) "not_attempted" else "inconclusive",
    extraction_status = if (is.null(error)) "not_attempted" else "failed",
    error = error
  )
}
