application_env <- new.env(parent = baseenv())
sys.source(
  file.path(project_root(), "R", "application", "ports.R"),
  envir = application_env
)

for (application_file in c("excipient_taxonomy.R", "excipient_matching.R")) {
  sys.source(
    file.path(project_root(), "R", "application", application_file),
    envir = application_env
  )
}
