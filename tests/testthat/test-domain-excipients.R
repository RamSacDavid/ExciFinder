test_that("excipients have caller-supplied identity and empty vocabularies", {
  excipient <- domain_env$new_excipient(
    id = "excipient-001",
    canonical_name = "Lactose"
  )

  same_name <- domain_env$new_excipient(
    id = "independent-id",
    canonical_name = "Lactose"
  )

  expect_s3_class(excipient, "excipient")
  expect_length(excipient$synonyms, 0L)
  expect_length(excipient$language_variants, 0L)
  expect_length(excipient$e_codes, 0L)
  expect_false(identical(excipient$id, same_name$id))
})

test_that("source documents accept partial traceability metadata", {
  document <- domain_env$new_source_document(
    id = "document-001",
    source = "official-source",
    subject_id = "opaque-formulation-id",
    document_type = "technical-document",
    retrieved_at = as.POSIXct("2026-01-01", tz = "UTC"),
    language = "es"
  )

  expect_s3_class(document, "source_document")
  expect_null(document$url)
  expect_null(document$document_date)
})

test_that("evidence keeps factual references and text", {
  evidence <- domain_env$new_excipient_evidence(
    id = "evidence-001",
    excipient_id = "excipient-001",
    subject_id = "opaque-formulation-id",
    document_id = "document-001",
    matched_term = "lactosa monohidrato",
    section = "6.1",
    excerpt = "Excipientes: lactosa monohidrato",
    method = "literal"
  )

  expect_s3_class(evidence, "excipient_evidence")
  expect_equal(evidence$document_id, "document-001")
  expect_error(
    domain_env$new_excipient_evidence(
      "evidence-002", "", "subject", "document", "term",
      excerpt = "text", method = "literal"
    ),
    "excipient_id"
  )
  expect_error(
    domain_env$new_excipient_evidence(
      "evidence-003", "excipient", "subject", "", "term",
      excerpt = "text", method = "literal"
    ),
    "document_id"
  )
})

test_that("factual domain objects contain no clinical interpretation", {
  excipient <- domain_env$new_excipient("excipient-001", "Lactose")
  evidence <- domain_env$new_excipient_evidence(
    "evidence-001", "excipient-001", "subject-001", "document-001",
    "lactose", excerpt = "Contains lactose", method = "literal"
  )
  assessment <- domain_env$new_excipient_assessment(
    "subject-001", "excipient-001", "identified", "complete",
    matcher_version = "1", taxonomy_version = "1"
  )
  clinical_fields <- c(
    "risk", "contraindicated", "safe", "recommendation",
    "population_warning"
  )

  expect_false(any(clinical_fields %in% names(excipient)))
  expect_false(any(clinical_fields %in% names(evidence)))
  expect_false(any(clinical_fields %in% names(assessment)))
})
