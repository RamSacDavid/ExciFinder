test_that("app.R parses and loads without a CIMA request", {
  expect_no_error(parse(file = app_path))
  files_before <- sort(list.files(
    project_root(),
    all.files = TRUE,
    recursive = TRUE,
    no.. = TRUE
  ))

  testthat::local_mocked_bindings(
    GET = function(...) {
      stop("Unexpected network request while loading app.R", call. = FALSE)
    },
    RETRY = function(...) {
      stop("Unexpected network request while loading app.R", call. = FALSE)
    },
    .package = "httr"
  )
  testthat::local_mocked_bindings(
    browseURL = function(...) {
      stop("Unexpected browser launch while loading app.R", call. = FALSE)
    },
    .package = "utils"
  )
  app_env <- new.env(parent = globalenv())

  loaded <- expect_no_error(source_app(app_env))
  expect_s3_class(loaded$value, "shiny.appobj")
  expect_true(exists("ui", envir = app_env, inherits = FALSE))
  expect_true(exists("server", envir = app_env, inherits = FALSE))
  expect_identical(
    sort(list.files(
      project_root(),
      all.files = TRUE,
      recursive = TRUE,
      no.. = TRUE
    )),
    files_before
  )
})

test_that("production app has no legacy search references", {
  app_text <- paste(readLines(app_path, warn = FALSE), collapse = "\n")

  expect_false(grepl("buscar_excipiente_legacy", app_text, fixed = TRUE))
  expect_false(grepl("cache_pdf", app_text, fixed = TRUE))
  expect_false(grepl("pdf_text", app_text, fixed = TRUE))
  expect_false(grepl("text_normalization.R", app_text, fixed = TRUE))
  expect_false(grepl("cima_legacy.R", app_text, fixed = TRUE))
  expect_false(grepl("excipient_search_legacy.R", app_text, fixed = TRUE))
  expect_false(grepl("tests/legacy", app_text, fixed = TRUE))
  expect_false(grepl("rsconnect", app_text, fixed = TRUE))
})

test_that("production R tree contains no legacy or pdftools fallback", {
  production_files <- c(
    app_path,
    list.files(
      file.path(project_root(), "R"),
      pattern = "[.]R$",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  production_text <- paste(vapply(production_files, function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)), collapse = "\n")

  expect_false(any(grepl("legacy", basename(production_files), ignore.case = TRUE)))
  expect_false(grepl("pdftools", production_text, fixed = TRUE))
  expect_false(grepl("pdf_text", production_text, fixed = TRUE))
  expect_false(grepl("tests/legacy", production_text, fixed = TRUE))
})
