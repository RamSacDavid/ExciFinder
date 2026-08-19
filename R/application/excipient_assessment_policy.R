.excipient_policy_assert_errors <- function(errors, name) {
  if (!is.list(errors)) {
    .excipient_application_abort(
      sprintf("`%s` must be a list.", name)
    )
  }
  if (!all(vapply(errors, function(error) {
      is.list(error) || is.character(error)
    }, logical(1)))) {
    .excipient_application_abort(
      sprintf("Every item in `%s` must be a list or character value.", name)
    )
  }
}

.excipient_policy_attempted <- function(
    structured_snapshots,
    smpc_artifact,
    smpc_content,
    structured_errors,
    smpc_error) {
  length(structured_snapshots) > 0L || length(structured_errors) > 0L ||
    !is.null(smpc_artifact) || !is.null(smpc_content) || !is.null(smpc_error)
}

assess_excipient_from_retrieved_sources <- function(
    subject_id,
    excipient,
    taxonomy_version,
    matcher_version,
    structured_snapshots = list(),
    smpc_artifact = NULL,
    smpc_content = NULL,
    structured_errors = list(),
    smpc_error = NULL) {
  .excipient_assert_non_empty_string(subject_id, "subject_id")
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  .excipient_assert_non_empty_string(taxonomy_version, "taxonomy_version")
  .excipient_assert_non_empty_string(matcher_version, "matcher_version")
  if (!is.list(structured_snapshots) || !all(vapply(
      structured_snapshots,
      is_source_composition_snapshot,
      logical(1)))) {
    .excipient_application_abort(
      "`structured_snapshots` must contain only `source_composition_snapshot` objects."
    )
  }
  .excipient_policy_assert_errors(structured_errors, "structured_errors")
  if (!is.null(smpc_error) && !is.list(smpc_error) && !is.character(smpc_error)) {
    .excipient_application_abort(
      "`smpc_error` must be NULL, a list, or a character value."
    )
  }
  if ((!is.null(smpc_artifact) || !is.null(smpc_content)) && !is.null(smpc_error)) {
    .excipient_application_abort(
      "Retrieved SmPC inputs and `smpc_error` are mutually exclusive."
    )
  }

  structured_attempts <- c(
    lapply(structured_snapshots, function(snapshot) {
      build_structured_excipient_attempt(snapshot, excipient, subject_id)
    }),
    lapply(structured_errors, function(error) {
      build_unavailable_excipient_attempt(
      source = "structured_composition",
      method = "structured_composition_controlled_terms",
        error = error
      )
    })
  )
  if (length(structured_attempts) == 0L) {
    structured_attempts <- list(build_unavailable_excipient_attempt(
      source = "structured_composition",
      method = "structured_composition_controlled_terms"
    ))
  }
  smpc_attempt <- if (is.null(smpc_artifact) && is.null(smpc_content)) {
    build_unavailable_excipient_attempt(
      source = "summary_of_product_characteristics",
      method = "section_6_1_controlled_terms",
      section = "6.1",
      error = smpc_error
    )
  } else {
    build_smpc_61_excipient_attempt(
      smpc_artifact,
      smpc_content,
      excipient,
      subject_id
    )
  }

  structured_positive <- any(vapply(structured_attempts, function(attempt) {
    identical(attempt$outcome, "evidence_found")
  }, logical(1)))
  structured_processed <- any(vapply(structured_attempts, function(attempt) {
    identical(attempt$extraction_status, "complete")
  }, logical(1)))
  smpc_positive <- identical(smpc_attempt$outcome, "evidence_found")
  smpc_exhaustive_no_match <- identical(smpc_attempt$outcome, "no_evidence") &&
    identical(smpc_attempt$extraction_status, "complete")
  smpc_complete <- smpc_positive || smpc_exhaustive_no_match

  # This version assesses only MedicinalProduct scope. A product-level result
  # does not establish composition for each formulation or presentation.
  factual_conclusion <- if (smpc_positive) {
    "identified"
  } else if (smpc_exhaustive_no_match && structured_positive) {
    "conflicting"
  } else if (smpc_exhaustive_no_match) {
    "not_identified"
  } else if (structured_positive) {
    "identified"
  } else {
    "indeterminate"
  }

  verification_coverage <- if (smpc_complete) {
    "complete"
  } else if (structured_processed) {
    "partial"
  } else if (.excipient_policy_attempted(
      structured_snapshots,
      smpc_artifact,
      smpc_content,
      structured_errors,
      smpc_error)) {
    "failed"
  } else {
    "not_attempted"
  }

  new_excipient_assessment(
    subject_id = subject_id,
    excipient_id = excipient$id,
    factual_conclusion = factual_conclusion,
    verification_coverage = verification_coverage,
    attempts = c(structured_attempts, list(smpc_attempt)),
    technical_errors = list(),
    matcher_version = matcher_version,
    taxonomy_version = taxonomy_version
  )
}
