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

excifinder_state_box <- function(conclusion, output_id) {
  titles <- c(
    identified = "IDENTIFICADO",
    not_identified = "NO IDENTIFICADO EN FUENTES VERIFICADAS",
    indeterminate = "NO VERIFICABLE",
    conflicting = "FUENTES DISCORDANTES"
  )
  shiny::div(
    class = paste(
      "excifinder-state-group",
      excifinder_state_class(conclusion)
    ),
    shiny::div(
      class = "excifinder-state-card",
      shiny::div(
        class = "excifinder-state-header",
        unname(titles[[conclusion]])
      ),
      shiny::div(
        class = "excifinder-state-body",
        DT::DTOutput(output_id)
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
        shiny::uiOutput("search_method"),
        shiny::uiOutput("search_message"),
        shiny::uiOutput("partial_warning"),
        shiny::uiOutput("partial_errors")
      ),
      shiny::div(
        class = "excifinder-results",
        shiny::uiOutput("result_groups")
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
