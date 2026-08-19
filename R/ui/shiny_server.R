excifinder_safe_filename <- function(active_ingredient, max_stem_chars = 80L) {
  if (!is.character(active_ingredient) || length(active_ingredient) != 1L ||
      is.na(active_ingredient)) {
    active_ingredient <- ""
  }
  stem <- stringi::stri_trans_nfkc(active_ingredient)
  stem <- stringi::stri_replace_all_regex(
    stem,
    "[\\\\/:*?\"<>|\\p{Cc}]+",
    " "
  )
  stem <- stringi::stri_replace_all_regex(
    stem,
    "[^\\p{L}\\p{N}._ -]+",
    " "
  )
  stem <- stringi::stri_trim_both(stem)
  stem <- stringi::stri_replace_all_regex(stem, "[\\p{Z}\\s]+", "_")
  stem <- stringi::stri_replace_all_regex(stem, "^[._-]+|[._-]+$", "")
  stem <- stringi::stri_sub(stem, 1L, as.integer(max_stem_chars))
  stem <- stringi::stri_replace_all_regex(stem, "[._-]+$", "")
  if (!nzchar(stem) || grepl(
      "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$",
      stem,
      ignore.case = TRUE)) {
    stem <- "busqueda"
  }
  paste0("ExciFinder_", stem, ".xlsx")
}

excifinder_dt_escape_columns <- function(table) {
  setdiff(names(table), "Ficha técnica")
}

build_excifinder_server <- function(search_service) {
  if (!is.list(search_service) || !is.function(search_service$search_excipient)) {
    stop("`search_service` must expose `search_excipient()`.", call. = FALSE)
  }

  function(input, output, session) {
    latest_result <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$buscar, {
      validation_error <- tryCatch(
        {
          validate_search_input_text(input$pa, "El principio activo")
          validate_search_input_text(input$excipiente, "El excipiente")
          NULL
        },
        error = function(error) conditionMessage(error)
      )
      if (!is.null(validation_error)) {
        shiny::showNotification(validation_error, type = "error")
        return()
      }
      result <- tryCatch(
        search_service$search_excipient(
          active_ingredient = input$pa,
          excipient_query = input$excipiente,
          filters = list(authorized = TRUE, marketed = TRUE)
        ),
        error = function(error) {
          shiny::showNotification(
            "No fue posible completar la búsqueda. Inténtelo de nuevo.",
            type = "error"
          )
          NULL
        }
      )
      if (!is.null(result)) {
        latest_result(result)
      }
    }, ignoreInit = TRUE)

    output$search_method <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      message <- present_search_messages(result)$method
      if (is.null(message)) return(NULL)
      shiny::div(class = "excifinder-message", message)
    })

    output$search_message <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      message <- present_search_messages(result)$primary
      if (is.null(message)) return(NULL)
      shiny::div(class = "excifinder-message", message)
    })

    output$partial_warning <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      message <- present_search_messages(result)$warning
      if (is.null(message)) return(NULL)
      shiny::div(class = "excifinder-warning", message)
    })

    output$partial_errors <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      errors <- present_partial_errors(result)
      if (nrow(errors) == 0L || length(result$results) == 0L) return(NULL)
      shiny::tags$details(
        shiny::tags$summary("Detalles de verificación"),
        shiny::tags$ul(lapply(seq_len(nrow(errors)), function(index) {
          shiny::tags$li(paste(
            errors$Medicamento_ID[[index]],
            "·", errors$Etapa[[index]],
            "·", errors$Mensaje[[index]]
          ))
        }))
      )
    })

    output$results_table <- DT::renderDT({
      result <- latest_result()
      shiny::req(result)
      table <- present_search_table(result)
      shiny::req(nrow(table) > 0L)
      DT::datatable(
        table,
        rownames = FALSE,
        escape = excifinder_dt_escape_columns(table),
        options = list(pageLength = 15L, autoWidth = TRUE)
      )
    })

    download_data <- shiny::reactive({
      result <- latest_result()
      shiny::req(result)
      present_search_export(result)
    })

    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        excifinder_safe_filename(input$pa)
      },
      content = function(file) {
        openxlsx::write.xlsx(download_data(), file)
      }
    )
  }
}
