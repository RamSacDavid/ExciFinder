application_env <- new.env(parent = baseenv())
sys.source(
  file.path(project_root(), "R", "application", "ports.R"),
  envir = application_env
)
