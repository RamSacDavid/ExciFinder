test_that("app.R parses and loads without a CIMA request", {
  expect_no_error(parse(file = app_path))

  testthat::local_mocked_bindings(
    GET = function(...) {
      stop("Unexpected network request while loading app.R", call. = FALSE)
    },
    .package = "httr"
  )
  app_env <- new.env(parent = globalenv())

  loaded <- expect_no_error(source_app(app_env))
  expect_s3_class(loaded$value, "shiny.appobj")
  expect_true(exists("ui", envir = app_env, inherits = FALSE))
  expect_true(exists("server", envir = app_env, inherits = FALSE))
})

test_that("production app has no legacy search references", {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("buscar_excipiente_legacy", app_text, fixed = TRUE))
  expect_false(grepl("cache_pdf", app_text, fixed = TRUE))
  expect_false(grepl("pdf_text", app_text, fixed = TRUE))
  expect_false(grepl("text_normalization.R", app_text, fixed = TRUE))
  expect_false(grepl("cima_legacy.R", app_text, fixed = TRUE))
  expect_false(grepl("excipient_search_legacy.R", app_text, fixed = TRUE))
})
