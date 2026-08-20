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
    class = paste("excifinder-state-group", excifinder_state_class(conclusion)),
    shinydashboard::box(
      title = unname(titles[[conclusion]]),
      solidHeader = TRUE,
      width = NULL,
      DT::DTOutput(output_id)
    )
  )
}

build_excifinder_ui <- function() {
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(title = "ExciFinder"),
    sidebar = shinydashboard::dashboardSidebar(
      shiny::div(
        style = "padding: 15px;",
        excifinder_predictive_input("pa", "Principio activo:", "pa_query"),
        excifinder_predictive_input(
          "excipiente", "Excipiente:", "excipiente_query"
        ),
        shiny::p(
          class = "excifinder-input-help",
          "Las sugerencias de excipiente proceden de datos estructurados disponibles y pueden no ser exhaustivas."
        ),
        shiny::actionButton(
          "buscar",
          "BUSCAR",
          icon = shiny::icon("search"),
          style = paste(
            "background-color: #004EB3; color: white; border: none;",
            "font-weight: bold; width: 100%; height: 40px;"
          )
        ),
        shiny::p(
          style = "margin-top: 10px; font-size: 12px;",
          "Se consultan medicamentos autorizados y comercializados."
        ),
        shiny::hr(),
        shiny::downloadButton(
          "downloadData",
          "Exportar Excel",
          style = "width: 100%;"
        ),
        shiny::br(), shiny::br(),
        shiny::div(
          style = "color: #ffffff; font-size: 11px; line-height: 1.4;",
          shiny::tags$p(shiny::tags$b("Aviso:")),
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
    ),
    body = shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$style(shiny::HTML("\
        body, .main-header .logo, .main-header .navbar, .main-sidebar, .content-header {
          font-family: Verdana, sans-serif !important;
        }
        .main-header .logo, .main-header .navbar { background-color: #004EB3 !important; }
        .content-wrapper, .right-side { background-color: #B8BEC4 !important; }
        .box-header .box-title { color: #ffffff !important; font-weight: bold; }
        .dataTables_wrapper { font-size: 12px; background: white; padding: 10px; }
        .excifinder-message { margin: 8px 0; font-weight: 600; }
        .excifinder-warning { margin: 8px 0; color: #7a4b00; font-weight: 600; }
        .excifinder-input-help { color: #e9eef5; font-size: 11px; line-height: 1.35; }
        .excifinder-state-group { width: 100%; overflow: hidden; }
        .excifinder-state-group .box { border-top: 0; background: #ffffff; width: 100%; }
        .excifinder-state-group .box-header { color: #ffffff; }
        .excifinder-state-group .dataTables_wrapper { width: 100%; overflow: hidden; }
        .excifinder-state-group .dataTables_scroll { max-width: 100%; }
        .excifinder-state-group table.dataTable { width: 100% !important; }
        .state-identified .box { background: #fbe9e9; }
        .state-identified .box-header { background: #a82d35; }
        .state-not-identified .box { background: #e8f5ec; }
        .state-not-identified .box-header { background: #287a45; }
        .state-indeterminate .box { background: #eeeeee; }
        .state-indeterminate .box-header { background: #5f6870; }
        .state-conflicting .box { background: #fff0df; }
        .state-conflicting .box-header { background: #b85f00; }
      "))),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::p(
            "La evaluación actual se realiza a nivel del medicamento autorizado (número de registro), no de cada presentación comercial individual."
          ),
          shiny::uiOutput("search_method"),
          shiny::uiOutput("search_message"),
          shiny::uiOutput("partial_warning"),
          shiny::uiOutput("partial_errors")
        )
      ),
      shiny::uiOutput("result_groups")
    )
  )
}
