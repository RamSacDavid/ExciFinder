test_that("release version has one canonical production source", {
  version_env <- new.env(parent = baseenv())
  sys.source(
    file.path(project_root(), "R", "version.R"),
    envir = version_env
  )
  sys.source(
    file.path(project_root(), "R", "adapters", "http_policy.R"),
    envir = version_env
  )

  expect_identical(version_env$excifinder_version(), "2.1.0")
  expect_identical(
    version_env$excifinder_http_user_agent(),
    paste0("ExciFinder/", version_env$excifinder_version())
  )

  production_files <- c(
    file.path(project_root(), "app.R"),
    list.files(
      file.path(project_root(), "R"),
      pattern = "[.]R$",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  production_text <- paste(vapply(production_files, function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")

  expect_false(grepl("ExciFinder/1.0", production_text, fixed = TRUE))
})

test_that("release licensing metadata is present", {
  license_path <- file.path(project_root(), "LICENSE")
  readme <- paste(readLines(
    file.path(project_root(), "README.md"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")

  expect_true(file.exists(license_path))
  expect_match(readme, "AGPL-3.0-only", fixed = TRUE)
})
