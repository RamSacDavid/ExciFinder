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
    "suggestions.R",
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
    "cima_suggestions.R",
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

new_excifinder_session_services <- function() {
  cima_client <- new_memoized_cima_client(new_cima_client())
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
  factual_service <- new_excipient_search_service(
    product_source = product_source,
    composition_source = composition_source,
    artifact_source = artifact_source,
    taxonomy = taxonomy,
    matcher_version = "controlled-term-v1",
    allow_literal_fallback = TRUE
  )
  contextual_service <- new_excipient_suggestion_service(
    product_source,
    composition_source
  )

  list(
    search_service = list(search_excipient = function(...) {
      args <- list(...)
      with_cima_client_cache_scope(
        cima_client,
        function() do.call(factual_service$search_excipient, args)
      )
    }),
    active_ingredient_suggestion_source =
      new_cima_active_ingredient_suggestion_source(),
    excipient_suggestion_service = list(
      suggest_excipients_for_active_ingredient = function(...) {
        args <- list(...)
        with_cima_client_cache_scope(
          cima_client,
          function() do.call(
            contextual_service$suggest_excipients_for_active_ingredient,
            args
          )
        )
      }
    )
  )
}

ui <- build_excifinder_ui()
server <- build_excifinder_server(service_factory = new_excifinder_session_services)

shiny::shinyApp(ui, server)
