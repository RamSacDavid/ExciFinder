# ============================================================
# EXCIFINDER v1.0: VALL D'HEBRON (FINAL VERSION)
# ============================================================

library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(stringi)
library(pdftools)
library(openxlsx)

cache_pdf <- new.env(parent = emptyenv())

source("R/text_normalization.R", local = TRUE)
source("R/cima_legacy.R", local = TRUE)
source("R/excipient_search_legacy.R", local = TRUE)

ui <- dashboardPage(
  header = dashboardHeader(title = "ExciFinder v.1.0"),
  sidebar = dashboardSidebar(
    div(style="padding: 15px;", 
        textInput("pa", "Principio Activo:", value = ""),
        textInput("excipiente", "Excipiente:", value = ""),
        numericInput("limite", "Máx. Medicamentos:", value = 15, min = 1, max = 50),
        br(),
        actionButton("buscar", "BUSCAR", icon = icon("search"), 
                     style="background-color: #004EB3; color: white; border: none; font-weight: bold; width: 100%; height: 40px;"),
        
        hr(),
        downloadButton("downloadData", "Exportar Excel", style="width: 100%;"),
        br(), br(),
        div(style="color: #ffffff; font-size: 11px; line-height: 1.4;",
            tags$p(tags$b("Aviso Legal:")),
            tags$p("Información basada en la API de la AEMPS. ExciFinder es una herramienta de apoyo. Verifique siempre con la Ficha Técnica oficial.")
        )
    )
  ),
  body = dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Tipografía Verdana */
        body, .main-header .logo, .main-header .navbar, .main-sidebar, .content-header {
          font-family: 'Verdana', sans-serif !important;
        }
        
        /* Color Primario Azul Campus */
        .main-header .logo, .main-header .navbar { 
          background-color: #004EB3 !important; 
        }
        
        /* Fondo Gris Pálido */
        .content-wrapper, .right-side {
          background-color: #B8BEC4 !important;
        }
        
        /* Cabeceras de cajas en blanco */
        .box-header .box-title {
          color: #ffffff !important;
          font-weight: bold;
        }
        
        .box.status-danger { border-top-color: #cc0000 !important; }
        .box.status-success { border-top-color: #28a745 !important; }
        .box { border-radius: 0px; box-shadow: 2px 2px 5px rgba(0,0,0,0.1); }
        
        /* Tablas */
        .dataTables_wrapper { font-size: 12px; background: white; padding: 10px; border-radius: 4px; }
      "))
    ),
    fluidRow(
      column(width = 6,
             box(title = "CONTIENE EXCIPIENTE", status = "danger", solidHeader = TRUE, width = NULL,
                 DTOutput("tabla_si"))
      ),
      column(width = 6,
             box(title = "NO CONTIENE EXCIPIENTE", status = "success", solidHeader = TRUE, width = NULL,
                 DTOutput("tabla_no"))
      )
    )
  )
)

server <- function(input, output, session) {
  data_final <- eventReactive(input$buscar, {
    req(input$pa, input$excipiente)
    
    buscar_excipiente_legacy(
      input$pa,
      input$excipiente,
      input$limite,
      cache_pdf
    )
  })
  
  output$tabla_si <- renderDT({
    req(data_final())
    df <- data_final() %>% filter(estado == TRUE) %>%
      mutate(Link = paste0("<a href='", url, "' target='_blank' style='color:#cc0000; font-weight:bold;'>[PDF]</a>")) %>%
      select(Link, Medicamento = nombre)
    datatable(df, escape = FALSE, rownames = FALSE, options = list(dom='tp', pageLength=15)) %>%
      formatStyle('Medicamento', color = '#cc0000', fontWeight = 'bold')
  })
  
  output$tabla_no <- renderDT({
    req(data_final())
    df <- data_final() %>% filter(estado == FALSE) %>%
      mutate(Link = paste0("<a href='", url, "' target='_blank' style='color:#28a745; font-weight:bold;'>[PDF]</a>")) %>%
      select(Medicamento = nombre, Link)
    datatable(df, escape = FALSE, rownames = FALSE, options = list(dom='tp', pageLength=15)) %>%
      formatStyle('Medicamento', color = '#28a745')
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0("ExciFinder_", input$pa, ".xlsx") },
    content = function(file) {
      write.xlsx(data_final() %>% select(Medicamento=nombre, Contiene=estado, URL_Ficha=url), file)
    }
  )
}

shinyApp(ui, server)
