.excipient_policy_assert_optional_error <- function(error, name) {
  if (!is.null(error) && !is.list(error) && !is.character(error)) {
    .excipient_application_abort(
      sprintf("`%s` must be NULL, a list, or a character value.", name)
    )
  }
}

.excipient_policy_attempted <- function(
    structured_snapshot,
    smpc_artifact,
    smpc_content,
    structured_error,
    smpc_error) {
  !is.null(structured_snapshot) || !is.null(smpc_artifact) ||
    !is.null(smpc_content) || !is.null(structured_error) || !is.null(smpc_error)
}

assess_excipient_from_retrieved_sources <- function(
    subject_id,
    excipient,
    taxonomy_version,
    matcher_version,
    structured_snapshot = NULL,
    smpc_artifact = NULL,
    smpc_content = NULL,
    structured_error = NULL,
    smpc_error = NULL) {
  .excipient_assert_non_empty_string(subject_id, "subject_id")
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  .excipient_assert_non_empty_string(taxonomy_version, "taxonomy_version")
  .excipient_assert_non_empty_string(matcher_version, "matcher_version")
  .excipient_policy_assert_optional_error(structured_error, "structured_error")
  .excipient_policy_assert_optional_error(smpc_error, "smpc_error")
  if (!is.null(structured_snapshot) && !is.null(structured_error)) {
    .excipient_application_abort(
      "`structured_snapshot` and `structured_error` are mutually exclusive."
    )
  }
  if ((!is.null(smpc_artifact) || !is.null(smpc_content)) && !is.null(smpc_error)) {
    .excipient_application_abort(
      "Retrieved SmPC inputs and `smpc_error` are mutually exclusive."
    )
  }

  structured_attempt <- if (is.null(structured_snapshot)) {
    build_unavailable_excipient_attempt(
      source = "structured_composition",
      method = "structured_composition_controlled_terms",
      error = structured_error
    )
  } else {
    build_structured_excipient_attempt(structured_snapshot, excipient, subject_id)
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

  structured_positive <- identical(structured_attempt$outcome, "evidence_found")
  structured_processed <- identical(structured_attempt$extraction_status, "complete")
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
      structured_snapshot,
      smpc_artifact,
      smpc_content,
      structured_error,
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
    attempts = list(structured_attempt, smpc_attempt),
    technical_errors = list(),
    matcher_version = matcher_version,
    taxonomy_version = taxonomy_version
  )
}
