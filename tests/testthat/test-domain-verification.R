make_domain_evidence <- function(
    id = "evidence-001",
    subject_id = "formulation-001",
    excipient_id = "excipient-001") {
  domain_env$new_excipient_evidence(
    id = id,
    excipient_id = excipient_id,
    subject_id = subject_id,
    document_id = "document-001",
    matched_term = "lactose",
    section = "6.1",
    excerpt = "Contains lactose",
    method = "literal"
  )
}

test_that("verification attempts accept every extraction status", {
  evidence <- make_domain_evidence()

  for (status in domain_env$verification_extraction_statuses()) {
    attempt <- domain_env$new_verification_attempt(
      source = "official-source",
      document_id = "document-001",
      method = "structured-section",
      outcome = "evidence_found",
      extraction_status = status,
      evidence = list(evidence)
    )
    expect_s3_class(attempt, "verification_attempt")
    expect_equal(attempt$extraction_status, status)
  }

  expect_error(
    domain_env$new_verification_attempt(
      source = "official-source",
      outcome = "inconclusive",
      extraction_status = "invalid"
    ),
    "extraction_status"
  )
})

test_that("verification attempts validate their evidence collection", {
  evidence <- make_domain_evidence()
  attempt <- domain_env$new_verification_attempt(
    source = "official-source",
    document_id = "document-001",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(evidence)
  )

  expect_identical(attempt$evidence[[1]], evidence)
  expect_error(
    domain_env$new_verification_attempt(
      source = "official-source",
      outcome = "no_evidence",
      extraction_status = "complete",
      evidence = list("not evidence")
    ),
    "excipient_evidence"
  )
})

test_that("assessments accept all conclusions and coverage values", {
  for (conclusion in domain_env$assessment_factual_conclusions()) {
    assessment <- domain_env$new_excipient_assessment(
      subject_id = "formulation-001",
      excipient_id = "excipient-001",
      factual_conclusion = conclusion,
      verification_coverage = "complete",
      matcher_version = "matcher-1",
      taxonomy_version = "taxonomy-1"
    )
    expect_equal(assessment$factual_conclusion, conclusion)
  }

  for (coverage in domain_env$verification_extraction_statuses()) {
    assessment <- domain_env$new_excipient_assessment(
      subject_id = "formulation-001",
      excipient_id = "excipient-001",
      factual_conclusion = "indeterminate",
      verification_coverage = coverage,
      matcher_version = "matcher-1",
      taxonomy_version = "taxonomy-1"
    )
    expect_equal(assessment$verification_coverage, coverage)
  }
})

test_that("indeterminate plus failed coverage is valid", {
  assessment <- domain_env$new_excipient_assessment(
    subject_id = "formulation-001",
    excipient_id = "excipient-001",
    factual_conclusion = "indeterminate",
    verification_coverage = "failed",
    technical_errors = list(list(code = "document_unavailable")),
    matcher_version = "matcher-1",
    taxonomy_version = "taxonomy-1"
  )

  expect_true(domain_env$is_indeterminate_assessment(assessment))
  expect_false(domain_env$is_identified_assessment(assessment))
})

test_that("unverifiable is not a factual conclusion", {
  expect_error(
    domain_env$new_excipient_assessment(
      subject_id = "formulation-001",
      excipient_id = "excipient-001",
      factual_conclusion = "unverifiable",
      verification_coverage = "failed",
      matcher_version = "matcher-1",
      taxonomy_version = "taxonomy-1"
    ),
    "factual_conclusion"
  )
})

test_that("assessments validate attempts and derive their evidence", {
  evidence <- make_domain_evidence()
  attempt <- domain_env$new_verification_attempt(
    source = "official-source",
    document_id = "document-001",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(evidence)
  )
  assessment <- domain_env$new_excipient_assessment(
    "formulation-001", "excipient-001", "identified", "complete",
    attempts = list(attempt),
    matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
  )

  expect_s3_class(assessment, "excipient_assessment")
  expect_identical(domain_env$assessment_evidence(assessment), list(evidence))
  expect_error(
    domain_env$new_excipient_assessment(
      "formulation-001", "excipient-001", "identified", "complete",
      attempts = list("not an attempt"),
      matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
    ),
    "verification_attempt"
  )
  expect_false("evidence" %in% names(assessment))
})

test_that("assessment evidence aggregates attempts in stable order", {
  first <- make_domain_evidence("evidence-001")
  second <- make_domain_evidence("evidence-002")
  first_attempt <- domain_env$new_verification_attempt(
    source = "structured-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(first)
  )
  second_attempt <- domain_env$new_verification_attempt(
    source = "pdf-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(second)
  )
  assessment <- domain_env$new_excipient_assessment(
    "formulation-001", "excipient-001", "identified", "complete",
    attempts = list(first_attempt, second_attempt),
    matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
  )

  expect_identical(domain_env$assessment_evidence(assessment), list(first, second))
})

test_that("assessment evidence is empty when attempts have no evidence", {
  attempt <- domain_env$new_verification_attempt(
    source = "structured-source",
    outcome = "no_evidence",
    extraction_status = "complete"
  )
  assessment <- domain_env$new_excipient_assessment(
    "formulation-001", "excipient-001", "not_identified", "complete",
    attempts = list(attempt),
    matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
  )

  expect_length(domain_env$assessment_evidence(assessment), 0L)
})

test_that("assessment evidence deduplicates repeated evidence IDs", {
  evidence <- make_domain_evidence("evidence-001")
  first_attempt <- domain_env$new_verification_attempt(
    source = "structured-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(evidence)
  )
  second_attempt <- domain_env$new_verification_attempt(
    source = "pdf-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(evidence)
  )
  assessment <- domain_env$new_excipient_assessment(
    "formulation-001", "excipient-001", "identified", "complete",
    attempts = list(first_attempt, second_attempt),
    matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
  )

  expect_length(domain_env$assessment_evidence(assessment), 1L)
})

test_that("assessments reject evidence for another subject or excipient", {
  wrong_subject <- make_domain_evidence(subject_id = "formulation-002")
  wrong_excipient <- make_domain_evidence(excipient_id = "excipient-002")
  subject_attempt <- domain_env$new_verification_attempt(
    source = "official-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(wrong_subject)
  )
  excipient_attempt <- domain_env$new_verification_attempt(
    source = "official-source",
    outcome = "evidence_found",
    extraction_status = "complete",
    evidence = list(wrong_excipient)
  )

  expect_error(
    domain_env$new_excipient_assessment(
      "formulation-001", "excipient-001", "identified", "complete",
      attempts = list(subject_attempt),
      matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
    ),
    "subject_id"
  )
  expect_error(
    domain_env$new_excipient_assessment(
      "formulation-001", "excipient-001", "identified", "complete",
      attempts = list(excipient_attempt),
      matcher_version = "matcher-1", taxonomy_version = "taxonomy-1"
    ),
    "excipient_id"
  )
})
