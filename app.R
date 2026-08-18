# ============================================================
# EXCIFINDER v1.0: VALL D'HEBRON (FINAL VERSION)
# ============================================================

libs <- c("shiny", "shinydashboard", "httr", "jsonlite", "dplyr", "tidyr", 
          "DT", "stringi", "pdftools", "openxlsx")
ins_libs <- libs[!(libs %in% installed.packages()[,"Package"])]
if(length(ins_libs)) install.packages(ins_libs)

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
  
  normalizar <- function(texto) {
    if (is.null(texto) || length(texto) == 0 || texto == "") return("")
    texto <- gsub("<[^>]*>", " ", texto)
    texto <- stri_trans_general(tolower(texto), "Latin-ASCII")
    texto <- gsub("z", "c", texto)
    return(texto)
  }
  
  data_final <- eventReactive(input$buscar, {
    req(input$pa, input$excipiente)
    
    res_pa <- GET("https://cima.aemps.es/cima/rest/medicamentos", query = list(practiv1 = input$pa))
    if (status_code(res_pa) != 200) return(NULL)
    meds <- fromJSON(rawToChar(res_pa$content))$resultados %>% head(input$limite)
    
    term_busqueda <- normalizar(input$excipiente)
    
    withProgress(message = 'Consultando CIMA...', value = 0, {
      results <- lapply(1:nrow(meds), function(i) {
        incProgress(1/nrow(meds), detail = "Analizando PDF de ficha técnica de CIMA AEMPS")
        nreg <- meds$nregistro[i]
        doc_ft <- meds$docs[[i]] %>% filter(tipo == 1)
        pdf_url <- if(nrow(doc_ft) > 0) doc_ft$url[1] else "#"
        
        found <- FALSE
        if (exists(nreg, envir = cache_pdf)) {
          if (grepl(term_busqueda, get(nreg, envir = cache_pdf))) found <- TRUE
        } else {
          url_doc <- paste0("https://cima.aemps.es/cima/rest/docSegmentado/contenido/1?nregistro=", nreg)
          res_doc <- GET(url_doc)
          txt <- ""
          if (status_code(res_doc) == 200) {
            sec61 <- fromJSON(rawToChar(res_doc$content)) %>% filter(seccion == "6.1")
            if (nrow(sec61) > 0) txt <- normalizar(sec61$contenido)
          }
          if (!grepl(term_busqueda, txt) && pdf_url != "#") {
            try({
              pdf_txt <- normalizar(paste(pdftools::pdf_text(pdf_url), collapse = " "))
              txt <- paste(txt, pdf_txt)
            }, silent = TRUE)
          }
          assign(nreg, txt, envir = cache_pdf)
          if (grepl(term_busqueda, txt)) found <- TRUE
        }
        return(data.frame(nombre = meds$nombre[i], estado = found, url = pdf_url))
      })
    })
    bind_rows(results) %>% arrange(nombre)
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
