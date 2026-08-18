verification_extraction_statuses <- function() {
  c("complete", "partial", "failed", "not_attempted")
}

verification_attempt_outcomes <- function() {
  c("evidence_found", "no_evidence", "inconclusive", "not_attempted")
}

assessment_factual_conclusions <- function() {
  c("identified", "not_identified", "indeterminate", "conflicting")
}

.domain_match_value <- function(value, allowed, name) {
  .domain_assert_non_empty_string(value, name)
  if (!value %in% allowed) {
    .domain_abort(sprintf(
      "`%s` must be one of: %s.",
      name,
      paste(allowed, collapse = ", ")
    ))
  }
  value
}

new_verification_attempt <- function(
    source,
    document_id = NULL,
    method = NULL,
    section = NULL,
    retrieved_at = NULL,
    outcome,
    extraction_status,
    error = NULL,
    evidence = list()) {
  .domain_assert_non_empty_string(source, "source")
  .domain_assert_optional_string(document_id, "document_id")
  .domain_assert_optional_string(method, "method")
  .domain_assert_optional_string(section, "section")
  outcome <- .domain_match_value(
    outcome,
    verification_attempt_outcomes(),
    "outcome"
  )
  extraction_status <- .domain_match_value(
    extraction_status,
    verification_extraction_statuses(),
    "extraction_status"
  )
  if (!is.null(error) && !is.list(error) && !is.character(error)) {
    .domain_abort("`error` must be NULL, a list, or a character value.")
  }
  evidence <- .domain_object_collection(
    evidence,
    "excipient_evidence",
    "evidence"
  )

  .new_domain_object(
    list(
      source = source,
      document_id = document_id,
      method = method,
      section = section,
      retrieved_at = retrieved_at,
      outcome = outcome,
      extraction_status = extraction_status,
      error = error,
      evidence = evidence
    ),
    "verification_attempt"
  )
}

new_excipient_assessment <- function(
    subject_id,
    excipient_id,
    factual_conclusion,
    verification_coverage,
    attempts = list(),
    technical_errors = list(),
    matcher_version,
    taxonomy_version) {
  .domain_assert_non_empty_string(subject_id, "subject_id")
  .domain_assert_non_empty_string(excipient_id, "excipient_id")
  factual_conclusion <- .domain_match_value(
    factual_conclusion,
    assessment_factual_conclusions(),
    "factual_conclusion"
  )
  verification_coverage <- .domain_match_value(
    verification_coverage,
    verification_extraction_statuses(),
    "verification_coverage"
  )
  attempts <- .domain_object_collection(
    attempts,
    "verification_attempt",
    "attempts"
  )
  for (attempt in attempts) {
    for (attempt_evidence in attempt$evidence) {
      if (!identical(attempt_evidence$subject_id, subject_id)) {
        .domain_abort(
          "Evidence subject_id must match the assessment subject_id."
        )
      }
      if (!identical(attempt_evidence$excipient_id, excipient_id)) {
        .domain_abort(
          "Evidence excipient_id must match the assessment excipient_id."
        )
      }
    }
  }
  if (!is.list(technical_errors)) {
    .domain_abort("`technical_errors` must be a list.")
  }
  .domain_assert_non_empty_string(matcher_version, "matcher_version")
  .domain_assert_non_empty_string(taxonomy_version, "taxonomy_version")

  .new_domain_object(
    list(
      subject_id = subject_id,
      excipient_id = excipient_id,
      factual_conclusion = factual_conclusion,
      verification_coverage = verification_coverage,
      attempts = attempts,
      technical_errors = unname(technical_errors),
      matcher_version = matcher_version,
      taxonomy_version = taxonomy_version
    ),
    "excipient_assessment"
  )
}

assessment_evidence <- function(x) {
  .domain_assert_class(x, "excipient_assessment", "x")

  evidence <- unlist(
    lapply(x$attempts, function(attempt) attempt$evidence),
    recursive = FALSE,
    use.names = FALSE
  )
  if (length(evidence) == 0L) {
    return(list())
  }

  evidence[!duplicated(vapply(evidence, function(item) item$id, character(1)))]
}

is_identified_assessment <- function(x) {
  .domain_assert_class(x, "excipient_assessment", "x")
  identical(x$factual_conclusion, "identified")
}

is_indeterminate_assessment <- function(x) {
  .domain_assert_class(x, "excipient_assessment", "x")
  identical(x$factual_conclusion, "indeterminate")
}
