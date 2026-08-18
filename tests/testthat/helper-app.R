project_root <- function() {
  path <- normalizePath(getwd(), mustWork = TRUE)

  while (!file.exists(file.path(path, "app.R"))) {
    parent <- dirname(path)
    if (identical(parent, path)) {
      stop("Could not locate the ExciFinder project root.", call. = FALSE)
    }
    path <- parent
  }

  path
}

app_path <- file.path(project_root(), "app.R")
fixtures_path <- file.path(project_root(), "tests", "fixtures", "cima")

source_app <- function(app_env) {
  original_directory <- getwd()
  on.exit(setwd(original_directory), add = TRUE)
  setwd(project_root())

  source(app_path, local = app_env)
}

load_app <- function() {
  app_env <- new.env(parent = globalenv())
  source_app(app_env)
  app_env
}

with_mocked_get <- function(app_env, mock_get, code) {
  had_get <- exists("GET", envir = app_env, inherits = FALSE)
  if (had_get) {
    original_get <- get("GET", envir = app_env, inherits = FALSE)
  }

  assign("GET", mock_get, envir = app_env)
  on.exit({
    if (had_get) {
      assign("GET", original_get, envir = app_env)
    } else {
      rm("GET", envir = app_env)
    }
  }, add = TRUE)

  force(code)
}

run_search <- function(mock_get, pa = "ejemplo", excipiente = "lactosa") {
  app_env <- load_app()
  result <- NULL

  with_mocked_get(app_env, mock_get, {
    shiny::testServer(app_env$server, {
      session$setInputs(
        pa = pa,
        excipiente = excipiente,
        limite = 15,
        buscar = 1
      )
      result <<- data_final()
    })
  })

  result
}
