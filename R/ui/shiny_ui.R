excifinder_text_input <- function(input_id, label) {
  shiny::div(
    class = "form-group shiny-input-container",
    shiny::tags$label(`for` = input_id, label),
    shiny::tags$input(
      id = input_id,
      type = "text",
      class = "shiny-input-text form-control",
      value = "",
      maxlength = as.character(search_input_max_chars())
    )
  )
}

build_excifinder_ui <- function() {
  shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(title = "ExciFinder"),
    sidebar = shinydashboard::dashboardSidebar(
      shiny::div(
        style = "padding: 15px;",
        excifinder_text_input("pa", "Principio activo:"),
        excifinder_text_input("excipiente", "Excipiente:"),
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
            "“No identificado” no debe interpretarse como garantía absoluta de ausencia",
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
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shinydashboard::box(
            title = "RESULTADOS",
            status = "primary",
            solidHeader = TRUE,
            width = NULL,
            DT::DTOutput("results_table")
          )
        )
      )
    )
  )
}
