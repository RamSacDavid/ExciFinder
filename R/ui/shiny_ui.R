excifinder_predictive_input <- function(input_id, label, query_input_id) {
  control <- shiny::selectizeInput(
    input_id,
    label,
    choices = character(),
    selected = NULL,
    options = list(
      create = TRUE,
      persist = FALSE,
      maxOptions = 15L,
      maxItems = 1L,
      onType = I(sprintf(
        "function(value) { Shiny.setInputValue('%s', value, {priority: 'event'}); }",
        query_input_id
      ))
    )
  )
  control$children[[2L]]$children[[1L]]$attribs$maxlength <-
    as.character(search_input_max_chars())
  control
}

excifinder_clinical_field <- function(label, value) {
  shiny::div(
    class = "excifinder-clinical-field",
    shiny::tags$dt(label),
    shiny::tags$dd(value)
  )
}

excifinder_master_item <- function(item, selected_product_id) {
  selected <- identical(item$id, selected_product_id)
  shiny::tags$button(
    type = "button",
    class = paste(
      "excifinder-master-item",
      if (selected) "is-selected" else NULL
    ),
    `data-product-id` = item$id,
    `aria-pressed` = if (selected) "true" else "false",
    `aria-current` = if (selected) "true" else NULL,
    onclick = paste0(
      "Shiny.setInputValue('selected_product_id', ",
      "this.dataset.productId, {priority: 'event'});"
    ),
    shiny::span(class = "excifinder-master-name", item$name),
    shiny::span(
      class = "excifinder-master-meta",
      paste(item$dose, "· N.º registro", item$registration_number)
    ),
    if (selected) shiny::span(
      class = "excifinder-selected-indicator",
      "Seleccionado"
    )
  )
}

excifinder_master_group <- function(group, selected_product_id) {
  shiny::tags$section(
    class = paste("excifinder-master-group", group$state_class),
    shiny::div(
      class = "excifinder-state-header",
      shiny::tags$h3(group$label),
      shiny::span(
        class = "excifinder-state-count",
        group$count,
        `aria-label` = paste(group$count, "medicamentos")
      )
    ),
    shiny::div(
      class = "excifinder-master-items",
      lapply(group$items, excifinder_master_item, selected_product_id)
    )
  )
}

excifinder_verification_attempt <- function(attempt) {
  shiny::div(
    class = "excifinder-verification-attempt",
    shiny::div(class = "excifinder-verification-source", attempt$source),
    shiny::div(class = "excifinder-verification-outcome", attempt$outcome),
    shiny::div(
      class = "excifinder-verification-extraction",
      attempt$extraction_status
    )
  )
}

excifinder_evidence_item <- function(evidence) {
  shiny::tags$article(
    class = "excifinder-evidence-item",
    shiny::tags$h4(evidence$source),
    shiny::p(
      class = "excifinder-matched-term",
      shiny::span("Término coincidente: "),
      evidence$matched_term
    ),
    shiny::tags$blockquote(evidence$excerpt)
  )
}

excifinder_evidence_block <- function(detail) {
  if (length(detail$evidence) == 0L) {
    return(shiny::p(
      class = "excifinder-no-evidence",
      detail$no_evidence_message
    ))
  }
  first <- excifinder_evidence_item(detail$evidence[[1L]])
  if (length(detail$evidence) == 1L) return(first)
  additional_count <- length(detail$evidence) - 1L
  shiny::tagList(
    first,
    shiny::tags$details(
      class = "excifinder-additional-evidence",
      shiny::tags$summary(paste(
        "Ver",
        additional_count,
        if (additional_count == 1L) "evidencia adicional" else "evidencias adicionales"
      )),
      lapply(detail$evidence[-1L], excifinder_evidence_item)
    )
  )
}

excifinder_clinical_detail <- function(detail) {
  shiny::tags$article(
    class = paste("excifinder-clinical-detail", detail$state_class),
    shiny::tags$header(
      class = "excifinder-detail-header",
      shiny::div(
        class = "excifinder-detail-heading",
        shiny::tags$h2(detail$name),
        shiny::p(
          class = "excifinder-active-ingredient",
          paste("Principio activo consultado:", detail$active_ingredient)
        )
      ),
      shiny::span(
        class = "excifinder-status-badge",
        detail$status_label
      ),
      shiny::div(
        class = "excifinder-registration",
        shiny::span("N.º registro"),
        shiny::strong(detail$registration_number)
      )
    ),
    shiny::tags$dl(
      class = "excifinder-clinical-fields",
      excifinder_clinical_field("Dosis", detail$fields$dose),
      excifinder_clinical_field(
        "Forma farmacéutica",
        detail$fields$pharmaceutical_form
      ),
      excifinder_clinical_field(
        "Vía de administración",
        detail$fields$administration_route
      ),
      excifinder_clinical_field(
        "Excipiente consultado",
        detail$fields$excipient
      ),
      excifinder_clinical_field(
        "Cobertura de verificación",
        detail$fields$coverage
      ),
      excifinder_clinical_field(
        "Fuentes verificadas",
        detail$fields$sources
      )
    ),
    shiny::tags$section(
      class = "excifinder-detail-section excifinder-verification-section",
      shiny::tags$h3("VERIFICACIÓN POR FUENTE"),
      if (length(detail$attempts) == 0L) {
        shiny::p("No se registraron intentos de verificación.")
      } else {
        lapply(detail$attempts, excifinder_verification_attempt)
      }
    ),
    shiny::tags$section(
      class = "excifinder-detail-section excifinder-evidence-section",
      shiny::tags$h3("EVIDENCIA"),
      excifinder_evidence_block(detail)
    ),
    if (length(detail$smpc_links) > 0L) shiny::div(
      class = "excifinder-smpc-links",
      lapply(detail$smpc_links, function(link) {
        shiny::tags$a(
          href = link$url,
          target = "_blank",
          rel = "noopener noreferrer",
          link$label
        )
      })
    )
  )
}

excifinder_result_browser <- function(browser) {
  count_label <- if (browser$context$product_count == 1L) {
    "1 medicamento evaluado"
  } else {
    paste(browser$context$product_count, "medicamentos evaluados")
  }
  shiny::tags$section(
    class = "excifinder-result-browser",
    `aria-labelledby` = "excifinder-results-title",
    shiny::tags$header(
      class = "excifinder-results-header",
      shiny::tags$h2(id = "excifinder-results-title", "Resultados"),
      shiny::p(
        class = "excifinder-results-query",
        paste(
          browser$context$active_ingredient,
          browser$context$excipient,
          sep = " · "
        )
      ),
      shiny::p(class = "excifinder-results-count", count_label),
      shiny::p(class = "excifinder-results-method", browser$context$method)
    ),
    shiny::div(
      class = "excifinder-master-detail",
      shiny::tags$nav(
        class = "excifinder-master",
        `aria-label` = "Lista de medicamentos",
        lapply(
          browser$groups,
          excifinder_master_group,
          browser$selected_product_id
        )
      ),
      shiny::div(
        class = "excifinder-detail-column",
        excifinder_clinical_detail(browser$detail)
      )
    )
  )
}

build_excifinder_ui <- function() {
  shiny::fluidPage(
    class = "excifinder-app",
    shiny::tags$head(
      shiny::tags$title("ExciFinder"),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "excifinder.css"
      )
    ),
    shiny::tags$header(
      class = "excifinder-header",
      shiny::div(
        class = "excifinder-header-inner",
        shiny::div(class = "excifinder-brand", "ExciFinder"),
        shiny::div(
          class = "excifinder-tagline",
          "Consulta de excipientes en medicamentos"
        )
      )
    ),
    shiny::tags$main(
      class = "excifinder-main",
      shiny::tags$section(
        class = "excifinder-search-card",
        shiny::div(
          class = "excifinder-search-intro",
          shiny::tags$h1("Consulta de excipientes"),
          shiny::tags$p(
            "Busca un principio activo y comprueba un excipiente en medicamentos autorizados."
          )
        ),
        shiny::div(
          class = "excifinder-search-form",
          shiny::div(
            class = "excifinder-form-field",
            excifinder_predictive_input(
              "pa", "Principio activo:", "pa_query"
            )
          ),
          shiny::div(
            class = "excifinder-form-field",
            excifinder_predictive_input(
              "excipiente", "Excipiente:", "excipiente_query"
            ),
            shiny::p(
              class = "excifinder-input-help",
              "Las sugerencias de excipiente proceden de datos estructurados disponibles y pueden no ser exhaustivas."
            )
          ),
          shiny::div(
            class = "excifinder-search-action",
            shiny::actionButton(
              "buscar",
              "BUSCAR",
              icon = shiny::icon("search"),
              class = "excifinder-button-primary"
            )
          )
        ),
        shiny::p(
          class = "excifinder-search-scope",
          "Se consultan medicamentos autorizados y comercializados."
        )
      ),
      shiny::tags$section(
        class = "excifinder-utility-row",
        shiny::div(
          class = "excifinder-scope-summary",
          shiny::tags$span(class = "excifinder-eyebrow", "Alcance"),
          shiny::tags$p(
            "Evaluación por medicamento autorizado (número de registro), no por presentación comercial individual."
          )
        ),
        shiny::div(
          class = "excifinder-export-action",
          shiny::downloadButton(
            "downloadData",
            "Exportar Excel",
            class = "excifinder-button-secondary"
          )
        )
      ),
      shiny::div(
        class = "excifinder-feedback",
        shiny::uiOutput("search_message"),
        shiny::uiOutput("partial_warning"),
        shiny::uiOutput("partial_errors")
      ),
      shiny::div(
        class = "excifinder-results",
        shiny::uiOutput("result_browser")
      ),
      shiny::tags$aside(
        class = "excifinder-clinical-notice",
        shiny::tags$h2("Aviso clínico"),
        shiny::tags$p(paste(
          "ExciFinder organiza información procedente de fuentes oficiales de CIMA/AEMPS.",
          "Los resultados reflejan únicamente las fuentes que pudieron analizarse.",
          "“No identificado en fuentes verificadas” no debe interpretarse como garantía absoluta de ausencia",
          "y la información debe verificarse en la ficha técnica oficial antes de tomar decisiones clínicas."
        )),
        shiny::tags$p(paste(
          "La evaluación actual es a nivel del medicamento autorizado,",
          "no de cada presentación individual."
        ))
      )
    )
  )
}
