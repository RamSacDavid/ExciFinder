source(file.path("R", "version.R"), local = TRUE)

for (domain_file in c(
    "medicinal_products.R",
    "excipients.R",
    "source_artifacts.R",
    "evidence.R",
    "verification.R")) {
  source(file.path("R", "domain", domain_file), local = TRUE)
}

for (application_file in c(
    "ports.R",
    "excipient_taxonomy.R",
    "literal_excipient.R",
    "excipient_matching.R",
    "excipient_evidence_builder.R",
    "excipient_assessment_policy.R",
    "search_excipient.R")) {
  source(file.path("R", "application", application_file), local = TRUE)
}

for (adapter_file in c(
    "http_policy.R",
    "cima_client.R",
    "cima_document_client.R",
    "cima_mapper.R",
    "cima_document_mapper.R",
    "cima_sources.R",
    "cima_document_sources.R")) {
  source(file.path("R", "adapters", adapter_file), local = TRUE)
}

for (ui_file in c(
    "search_presenter.R",
    "shiny_ui.R",
    "shiny_server.R")) {
  source(file.path("R", "ui", ui_file), local = TRUE)
}

cima_client <- new_cima_client()
document_client <- new_cima_document_client()
product_source <- new_cima_product_source_port(cima_client)
composition_source <- new_cima_composition_source_port(cima_client)
artifact_source <- new_cima_document_source_artifact_port(
  cima_client,
  document_client
)

taxonomy <- new_excipient_taxonomy(
  version = "bootstrap-v1",
  excipients = list()
)
search_service <- new_excipient_search_service(
  product_source = product_source,
  composition_source = composition_source,
  artifact_source = artifact_source,
  taxonomy = taxonomy,
  matcher_version = "controlled-term-v1",
  allow_literal_fallback = TRUE
)

ui <- build_excifinder_ui()
server <- build_excifinder_server(search_service)

shiny::shinyApp(ui, server)
