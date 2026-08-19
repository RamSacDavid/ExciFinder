application_env <- new.env(parent = baseenv())

for (domain_file in c(
    "medicinal_products.R",
    "excipients.R",
    "source_artifacts.R",
    "evidence.R",
    "verification.R")) {
  sys.source(
    file.path(project_root(), "R", "domain", domain_file),
    envir = application_env
  )
}

sys.source(
  file.path(project_root(), "R", "application", "ports.R"),
  envir = application_env
)

for (application_file in c(
    "excipient_taxonomy.R",
    "suggestions.R",
    "literal_excipient.R",
    "excipient_matching.R",
    "excipient_evidence_builder.R",
    "excipient_assessment_policy.R",
    "search_excipient.R")) {
  sys.source(
    file.path(project_root(), "R", "application", application_file),
    envir = application_env
  )
}

for (ui_file in c(
    "search_presenter.R",
    "shiny_ui.R",
    "shiny_server.R")) {
  sys.source(
    file.path(project_root(), "R", "ui", ui_file),
    envir = application_env
  )
}
