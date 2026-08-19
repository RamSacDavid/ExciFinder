test_that("factual policy implements the required decision table", {
  structured_match <- make_factual_structured_snapshot("Lactosa monohidrato")
  structured_no_match <- make_factual_structured_snapshot("Sacarosa")
  smpc_match <- make_factual_smpc("Contiene lactosa")
  smpc_no_match <- make_factual_smpc("Contiene sacarosa")
  smpc_html <- make_factual_smpc("<p>Contiene lactosa</p>", "text/html")
  smpc_empty <- make_factual_smpc("")

  cases <- list(
    A = assess_factual_fixture(structured_match, smpc_match),
    B = assess_factual_fixture(structured_no_match, smpc_match),
    C = assess_factual_fixture(structured_match, smpc_no_match),
    D = assess_factual_fixture(structured_no_match, smpc_no_match),
    E = assess_factual_fixture(NULL, smpc_no_match),
    F = assess_factual_fixture(
      structured_match,
      smpc_error = list(code = "section_unavailable")
    ),
    G = assess_factual_fixture(
      structured_no_match,
      smpc_error = list(code = "section_unavailable")
    ),
    H = assess_factual_fixture(
      structured_error = list(code = "structured_unavailable"),
      smpc_error = list(code = "section_unavailable")
    ),
    I = assess_factual_fixture(NULL, smpc_html),
    J = assess_factual_fixture(NULL, smpc_empty)
  )

  expect_identical(
    vapply(cases, `[[`, character(1), "factual_conclusion"),
    c(
      A = "identified",
      B = "identified",
      C = "conflicting",
      D = "not_identified",
      E = "not_identified",
      F = "identified",
      G = "indeterminate",
      H = "indeterminate",
      I = "indeterminate",
      J = "indeterminate"
    )
  )
  expect_identical(
    vapply(cases, `[[`, character(1), "verification_coverage"),
    c(
      A = "complete",
      B = "complete",
      C = "complete",
      D = "complete",
      E = "complete",
      F = "partial",
      G = "partial",
      H = "failed",
      I = "failed",
      J = "failed"
    )
  )
})

test_that("structured no-match never establishes factual absence", {
  assessment <- assess_factual_fixture(
    make_factual_structured_snapshot("Sacarosa")
  )

  expect_identical(assessment$factual_conclusion, "indeterminate")
  expect_identical(assessment$verification_coverage, "partial")
  expect_identical(assessment$attempts[[1]]$outcome, "no_evidence")
  expect_identical(assessment$attempts[[1]]$extraction_status, "complete")
})

test_that("unsupported and empty 6.1 never establish absence", {
  unsupported <- assess_factual_fixture(
    smpc = make_factual_smpc("<p>lactosa</p>", "text/html")
  )
  empty <- assess_factual_fixture(smpc = make_factual_smpc(""))

  expect_identical(unsupported$factual_conclusion, "indeterminate")
  expect_identical(empty$factual_conclusion, "indeterminate")
  expect_false(any(vapply(
    list(unsupported, empty),
    function(assessment) identical(assessment$factual_conclusion, "not_identified"),
    logical(1)
  )))
})

test_that("conflict is asymmetric according to source exhaustiveness", {
  structured_match_document_no_match <- assess_factual_fixture(
    make_factual_structured_snapshot("Lactosa"),
    make_factual_smpc("Sacarosa")
  )
  structured_no_match_document_match <- assess_factual_fixture(
    make_factual_structured_snapshot("Sacarosa"),
    make_factual_smpc("Lactosa")
  )

  expect_identical(
    structured_match_document_no_match$factual_conclusion,
    "conflicting"
  )
  expect_identical(
    structured_no_match_document_match$factual_conclusion,
    "identified"
  )
})

test_that("technical errors remain attached to their source attempts", {
  structured_error <- list(code = "structured_timeout")
  smpc_error <- list(code = "smpc_503")

  assessment <- assess_factual_fixture(
    structured_error = structured_error,
    smpc_error = smpc_error
  )

  expect_identical(assessment$attempts[[1]]$error, structured_error)
  expect_identical(assessment$attempts[[2]]$error, smpc_error)
  expect_length(assessment$technical_errors, 0L)
})

test_that("no source input is genuinely not attempted", {
  assessment <- assess_factual_fixture()

  expect_identical(assessment$factual_conclusion, "indeterminate")
  expect_identical(assessment$verification_coverage, "not_attempted")
  expect_true(all(vapply(assessment$attempts, function(attempt) {
    identical(attempt$outcome, "not_attempted") &&
      identical(attempt$extraction_status, "not_attempted")
  }, logical(1))))
})

test_that("multiblock SmPC supports product-level conclusions only", {
  multiblock <- read_cima_text_fixture("document-section-61-multiblock.txt")
  smpc <- make_factual_smpc(multiblock)
  present <- assess_factual_fixture(
    smpc = smpc,
    excipient = make_factual_excipient("excipient-x", "Excipiente X")
  )
  absent <- assess_factual_fixture(
    smpc = smpc,
    excipient = make_factual_excipient("excipient-w", "Excipiente W")
  )

  # Product-level assessment does not establish presentation-level composition.
  expect_identical(present$subject_id, "AEMPS:30001")
  expect_identical(present$factual_conclusion, "identified")
  expect_identical(absent$subject_id, "AEMPS:30001")
  expect_identical(absent$factual_conclusion, "not_identified")
  expect_identical(absent$verification_coverage, "complete")
})

test_that("assessment evidence remains nested in attempts and accessor-compatible", {
  assessment <- assess_factual_fixture(
    make_factual_structured_snapshot("Lactosa"),
    make_factual_smpc("También contiene lactosa")
  )
  evidence <- domain_env$assessment_evidence(assessment)

  expect_length(evidence, 2L)
  expect_true(all(vapply(evidence, function(item) {
    identical(item$subject_id, assessment$subject_id) &&
      identical(item$excipient_id, assessment$excipient_id)
  }, logical(1))))
  expect_false("evidence" %in% names(assessment))
})
