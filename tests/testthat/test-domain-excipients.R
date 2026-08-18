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

test_that("document source artifacts accept partial traceability metadata", {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-document-001",
    source = "official-source",
    subject_id = "opaque-formulation-id",
    artifact_type = "document",
    artifact_kind = "summary_of_product_characteristics",
    retrieved_at = as.POSIXct("2026-01-01", tz = "UTC"),
    language = "es"
  )

  expect_s3_class(artifact, "source_artifact")
  expect_equal(artifact$artifact_type, "document")
  expect_equal(artifact$artifact_kind, "summary_of_product_characteristics")
  expect_null(artifact$url)
  expect_null(artifact$source_date)
})

test_that("structured source artifacts can omit document metadata", {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-structured-001",
    source = "official-source",
    subject_id = "opaque-formulation-id",
    artifact_type = "structured_record",
    artifact_kind = "medicinal_product_record",
    version = "record-version-1"
  )

  expect_s3_class(artifact, "source_artifact")
  expect_equal(artifact$artifact_type, "structured_record")
  expect_equal(artifact$artifact_kind, "medicinal_product_record")
  expect_null(artifact$url)
  expect_error(
    domain_env$new_source_artifact(
      "artifact-invalid", "official-source", "subject-001", "invalid"
    ),
    "artifact_type"
  )
})

test_that("source artifact kinds are mandatory and remain open", {
  expect_error(
    domain_env$new_source_artifact(
      "artifact-empty-kind", "official-source", "subject-001", "document", ""
    ),
    "artifact_kind"
  )
  artifact <- domain_env$new_source_artifact(
    "artifact-future-kind", "official-source", "subject-001", "document",
    "regulatory_addendum"
  )

  expect_identical(artifact$artifact_kind, "regulatory_addendum")
})

test_that("document evidence keeps section references and text", {
  evidence <- domain_env$new_excipient_evidence(
    id = "evidence-001",
    excipient_id = "excipient-001",
    subject_id = "opaque-formulation-id",
    source_artifact_id = "artifact-document-001",
    matched_term = "lactose monohydrate",
    section = "6.1",
    excerpt = "Contains lactose monohydrate",
    method = "literal"
  )

  expect_s3_class(evidence, "excipient_evidence")
  expect_equal(evidence$source_artifact_id, "artifact-document-001")
  expect_equal(evidence$section, "6.1")
  expect_error(
    domain_env$new_excipient_evidence(
      "evidence-002", "", "subject", "artifact", "term",
      excerpt = "text", method = "literal"
    ),
    "excipient_id"
  )
  expect_error(
    domain_env$new_excipient_evidence(
      "evidence-003", "excipient", "subject", "", "term",
      excerpt = "text", method = "literal"
    ),
    "source_artifact_id"
  )
})

test_that("structured evidence does not require a section", {
  evidence <- domain_env$new_excipient_evidence(
    id = "evidence-structured-001",
    excipient_id = "excipient-001",
    subject_id = "opaque-formulation-id",
    source_artifact_id = "artifact-structured-001",
    matched_term = "lactose monohydrate",
    section = NULL,
    excerpt = "lactose monohydrate",
    method = "structured-entry",
    location = "composition-entry-1"
  )

  expect_s3_class(evidence, "excipient_evidence")
  expect_null(evidence$section)
})

test_that("factual domain objects contain no clinical interpretation", {
  excipient <- domain_env$new_excipient("excipient-001", "Lactose")
  evidence <- domain_env$new_excipient_evidence(
    "evidence-001", "excipient-001", "subject-001", "artifact-001",
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
