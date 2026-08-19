test_that("evidence IDs are deterministic and distinguish logical matches", {
  first_candidate <- application_env$new_excipient_match_candidate(
    "excipient-lactose",
    "lactosa",
    "Contiene lactosa",
    location = list(line = 2L)
  )
  another_location <- application_env$new_excipient_match_candidate(
    "excipient-lactose",
    "lactosa",
    "También contiene lactosa",
    location = list(line = 3L)
  )

  first <- application_env$build_excipient_evidence(
    first_candidate, "artifact-1", "AEMPS:30001", "6.1", 1L
  )
  repeated <- application_env$build_excipient_evidence(
    first_candidate, "artifact-1", "AEMPS:30001", "6.1", 1L
  )
  different_ordinal <- application_env$build_excipient_evidence(
    first_candidate, "artifact-1", "AEMPS:30001", "6.1", 2L
  )
  different_location <- application_env$build_excipient_evidence(
    another_location, "artifact-1", "AEMPS:30001", "6.1", 1L
  )

  expect_identical(first$id, repeated$id)
  expect_false(identical(first$id, different_ordinal$id))
  expect_false(identical(first$id, different_location$id))
  expect_true(startsWith(first$id, "excifinder:evidence:v1:"))
})

test_that("structured matches build product-level evidence with provenance", {
  snapshot <- make_factual_structured_snapshot(
    c("Lactosa monohidrato", "Sacarosa")
  )

  attempt <- application_env$build_structured_excipient_attempt(
    snapshot,
    make_factual_excipient(),
    "AEMPS:30001"
  )
  evidence <- attempt$evidence[[1]]

  expect_identical(attempt$outcome, "evidence_found")
  expect_identical(attempt$extraction_status, "complete")
  expect_identical(attempt$source_artifact_id, snapshot$source_artifact$id)
  expect_identical(evidence$subject_id, "AEMPS:30001")
  expect_false(identical(evidence$subject_id, snapshot$source_artifact$subject_id))
  expect_identical(evidence$source_artifact_id, snapshot$source_artifact$id)
  expect_identical(evidence$matched_term, "lactosa")
  expect_identical(evidence$excerpt, "Lactosa monohidrato")
  expect_identical(evidence$location$source_record_id, "entry-1")
  expect_null(evidence$section)
})

test_that("structured no-match is complete only for that non-exhaustive source", {
  attempt <- application_env$build_structured_excipient_attempt(
    make_factual_structured_snapshot("Sacarosa"),
    make_factual_excipient(),
    "AEMPS:30001"
  )

  expect_identical(attempt$outcome, "no_evidence")
  expect_identical(attempt$extraction_status, "complete")
  expect_length(attempt$evidence, 0L)
})

test_that("unavailable sources do not invent artifact identities", {
  absent <- application_env$build_unavailable_excipient_attempt(
    "structured_composition",
    "structured_composition_controlled_terms"
  )
  failed <- application_env$build_unavailable_excipient_attempt(
    "structured_composition",
    "structured_composition_controlled_terms",
    error = list(code = "source_unavailable")
  )

  expect_identical(absent$outcome, "not_attempted")
  expect_identical(absent$extraction_status, "not_attempted")
  expect_null(absent$source_artifact_id)
  expect_identical(failed$outcome, "inconclusive")
  expect_identical(failed$extraction_status, "failed")
  expect_identical(failed$error$code, "source_unavailable")
})

test_that("valid SmPC 6.1 builds documentary evidence and no-match attempts", {
  positive_smpc <- make_factual_smpc("Contiene Lactosa monohidrato")
  negative_smpc <- make_factual_smpc("Contiene sacarosa")
  excipient <- make_factual_excipient()

  positive <- application_env$build_smpc_61_excipient_attempt(
    positive_smpc$artifact,
    positive_smpc$content,
    excipient,
    "AEMPS:30001"
  )
  repeated <- application_env$build_smpc_61_excipient_attempt(
    positive_smpc$artifact,
    positive_smpc$content,
    excipient,
    "AEMPS:30001"
  )
  negative <- application_env$build_smpc_61_excipient_attempt(
    negative_smpc$artifact,
    negative_smpc$content,
    excipient,
    "AEMPS:30001"
  )
  evidence <- positive$evidence[[1]]

  expect_identical(positive$outcome, "evidence_found")
  expect_identical(positive$extraction_status, "complete")
  expect_identical(evidence$subject_id, "AEMPS:30001")
  expect_identical(evidence$source_artifact_id, positive_smpc$artifact$id)
  expect_identical(evidence$section, "6.1")
  expect_identical(evidence$matched_term, "lactosa")
  expect_identical(evidence$excerpt, "Contiene Lactosa monohidrato")
  expect_identical(evidence$location$line, 1L)
  expect_identical(evidence$id, repeated$evidence[[1]]$id)
  expect_identical(negative$outcome, "no_evidence")
  expect_identical(negative$extraction_status, "complete")
})

test_that("invalid or empty SmPC 6.1 inputs remain inconclusive", {
  excipient <- make_factual_excipient()
  invalid_inputs <- list(
    make_factual_smpc("lactosa", artifact_type = "structured_record"),
    make_factual_smpc("lactosa", artifact_kind = "package_leaflet"),
    make_factual_smpc("lactosa", content_artifact_id = "artifact-other"),
    make_factual_smpc("lactosa", section = "6.2"),
    make_factual_smpc("<p>lactosa</p>", content_type = "text/html"),
    make_factual_smpc("")
  )

  attempts <- lapply(invalid_inputs, function(input) {
    application_env$build_smpc_61_excipient_attempt(
      input$artifact,
      input$content,
      excipient,
      "AEMPS:30001"
    )
  })

  expect_true(all(vapply(attempts, function(attempt) {
    identical(attempt$outcome, "inconclusive") &&
      identical(attempt$extraction_status, "partial") &&
      length(attempt$evidence) == 0L
  }, logical(1))))
})
