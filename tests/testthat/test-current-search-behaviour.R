test_that("positive section 6.1 match is classified as present", {
  result <- run_search(make_cima_mock())

  expect_equal(result$estado, TRUE)
  expect_equal(result$nombre, "Medicamento de ejemplo 10 mg")
  expect_equal(result$url, "#")
})

test_that("matching normalizes case and diacritics", {
  result <- run_search(make_cima_mock(), excipiente = "LÁCTOSA")

  expect_equal(result$estado, TRUE)
})

test_that("z is transformed to c in the current normalizer", {
  # KNOWN BASELINE BEHAVIOUR — scheduled for deliberate replacement
  app_env <- load_legacy_engine()

  expect_equal(app_env$normalizar("Zinc"), "cinc")
})

test_that("a readable source without the term is classified as absent", {
  result <- run_search(make_cima_mock(documento = "doc-61-absent.json"))

  expect_equal(result$estado, FALSE)
})

test_that("a document failure is currently classified as absent", {
  # KNOWN UNSAFE BASELINE BEHAVIOUR
  #
  # Current ExciFinder conflates technical non-verification with absence.
  # This characterization test intentionally records the legacy behaviour.
  # It will be changed when tri-state result classification is introduced.
  result <- run_search(make_cima_mock(document_status = 503L))

  expect_equal(result$estado, FALSE)
})

test_that("an initial CIMA error produces the current NULL reactive result", {
  result <- run_search(make_cima_mock(initial_status = 503L))

  expect_null(result)
})

test_that("zero medicines exposes the legacy sequence defect", {
  # KNOWN BASELINE DEFECT — scheduled for deliberate correction
  expect_error(
    run_search(make_cima_mock(medicamentos = "medicamentos-zero.json")),
    "argument of length 0"
  )
})
