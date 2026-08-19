test_that("parallel validation cases declare the complete closed model", {
  cases <- parallel_validation_cases()
  required_fields <- c(
    "case_id", "description", "excipient_query", "legacy_expected",
    "new_expected", "comparison_expectation", "rationale"
  )

  expect_identical(vapply(cases, `[[`, character(1), "case_id"), LETTERS[1:20])
  expect_true(all(vapply(cases, function(case) {
    all(required_fields %in% names(case)) &&
      all(vapply(case[required_fields], function(value) {
        !is.null(value) && length(value) > 0L
      }, logical(1)))
  }, logical(1))))
  expect_true(all(vapply(cases, function(case) {
    case$comparison_expectation %in% c(
      "equivalent", "expected_improvement", "requires_review"
    )
  }, logical(1))))
  expect_true(all(vapply(cases, function(case) {
    identical(case$new_expected, case$gold_expected)
  }, logical(1))))
})

test_that("parallel matrix matches every legacy, new, and gold expectation", {
  matrix <- parallel_validation_matrix()

  expect_named(
    matrix,
    c(
      "case_id", "legacy_observed", "new_observed",
      "comparison_expectation", "comparison_passed"
    )
  )
  expect_identical(matrix$case_id, LETTERS[1:20])
  expect_true(all(matrix$comparison_passed))
  expect_identical(sum(matrix$comparison_expectation == "equivalent"), 7L)
  expect_identical(
    sum(matrix$comparison_expectation == "expected_improvement"),
    13L
  )
  expect_identical(sum(matrix$comparison_expectation == "requires_review"), 0L)
})

test_that("deliberate safety improvements are green comparisons", {
  matrix <- parallel_validation_matrix()
  improvements <- matrix[
    matrix$comparison_expectation == "expected_improvement",
    ,
    drop = FALSE
  ]

  expect_true(all(improvements$comparison_passed))
  expect_true(all(improvements$legacy_observed != improvements$new_observed))
  expect_identical(
    improvements$case_id,
    c("C", "D", "E", "F", "G", "H", "L", "O", "P", "Q", "R", "S", "T")
  )
})

test_that("new technical failures never become factual absence", {
  observations <- parallel_validation_observations()
  names(observations) <- vapply(observations, function(item) {
    item$case$case_id
  }, character(1))

  expect_false(any(vapply(observations[c("C", "Q", "S")], function(item) {
    identical(item$new$state, "new_not_identified")
  }, logical(1))))
  expect_identical(observations$C$new$state, "new_indeterminate")
  expect_identical(observations$Q$new$state, "new_indeterminate")
  expect_identical(observations$S$new$state, "new_indeterminate")
})

test_that("non-exhaustive structured no-match never establishes absence", {
  observations <- parallel_validation_observations()
  names(observations) <- vapply(observations, function(item) {
    item$case$case_id
  }, character(1))

  expect_identical(observations$E$new$state, "new_indeterminate")
  expect_identical(observations$E$new$coverage, "partial")
  expect_false(identical(observations$E$new$state, "new_not_identified"))
})

test_that("unsupported and empty content never establish absence", {
  observations <- parallel_validation_observations()
  names(observations) <- vapply(observations, function(item) {
    item$case$case_id
  }, character(1))
  empty <- run_parallel_new_scenario(
    list(document_text = "", document_mode = "valid"),
    "lactosa"
  )

  expect_identical(observations$P$new$state, "new_indeterminate")
  expect_identical(empty$state, "new_indeterminate")
  expect_false(observations$P$new$state == "new_not_identified")
  expect_false(empty$state == "new_not_identified")
})

test_that("ambiguous taxonomy stops before every source", {
  observations <- parallel_validation_observations()
  ambiguous <- Filter(function(item) {
    identical(item$case$case_id, "T")
  }, observations)[[1]]

  expect_identical(ambiguous$new$state, "new_query_ambiguous")
  expect_identical(ambiguous$new$source_calls, 0L)
  expect_identical(ambiguous$new$result_count, 0L)
})

test_that("literal fallback does not reproduce legacy fuzzy substitutions", {
  observations <- parallel_validation_observations()
  names(observations) <- vapply(observations, function(item) {
    item$case$case_id
  }, character(1))

  expect_identical(observations$F$legacy$state, "legacy_positive")
  expect_identical(observations$F$new$state, "new_not_identified")
  expect_identical(observations$F$new$strategy, "literal")
  expect_identical(observations$G$legacy$state, "legacy_positive")
  expect_identical(observations$G$new$state, "new_not_identified")
})

test_that("multiblock validation remains product-level only", {
  observations <- parallel_validation_observations()
  names(observations) <- vapply(observations, function(item) {
    item$case$case_id
  }, character(1))

  # This case validates product-level presence only.
  expect_identical(observations$M$new$state, "new_identified")
  expect_identical(observations$N$new$state, "new_not_identified")
  expect_match(observations$M$case$rationale, "product-level")
  expect_match(observations$N$case$rationale, "MedicinalProduct")
})
