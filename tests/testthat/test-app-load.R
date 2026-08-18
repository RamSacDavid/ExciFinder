test_that("app.R parses and loads without a CIMA request", {
  expect_no_error(parse(file = app_path))

  app_env <- new.env(parent = globalenv())
  assign(
    "GET",
    function(...) stop("Unexpected network request while loading app.R", call. = FALSE),
    envir = app_env
  )

  loaded <- expect_no_error(source_app(app_env))
  expect_s3_class(loaded$value, "shiny.appobj")
  expect_true(exists("ui", envir = app_env, inherits = FALSE))
  expect_true(exists("server", envir = app_env, inherits = FALSE))
})
