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

build_excifinder_server <- function(
    search_service = NULL,
    active_ingredient_suggestion_source = NULL,
    excipient_suggestion_service = NULL,
    service_factory = NULL) {
  valid_search_service <- function(service) {
    is.list(service) && is.function(service$search_excipient)
  }
  if (is.null(service_factory) && !valid_search_service(search_service)) {
    stop("`search_service` must expose `search_excipient()`.", call. = FALSE)
  }
  if (!is.null(service_factory) && !is.function(service_factory)) {
    stop("`service_factory` must be a function.", call. = FALSE)
  }

  suggestion_choices <- function(suggestions, current = NULL) {
    values <- vapply(suggestions, `[[`, character(1), "value")
    labels <- vapply(suggestions, `[[`, character(1), "label")
    choices <- stats::setNames(values, labels)
    if (is.character(current) && length(current) == 1L &&
        !is.na(current) && nzchar(current) && !current %in% values) {
      choices <- c(stats::setNames(current, current), choices)
    }
    choices
  }

  function(input, output, session) {
    services <- if (is.function(service_factory)) service_factory() else list(
      search_service = search_service,
      active_ingredient_suggestion_source = active_ingredient_suggestion_source,
      excipient_suggestion_service = excipient_suggestion_service
    )
    if (!valid_search_service(services$search_service)) {
      stop("Session services must expose a valid search service.", call. = FALSE)
    }
    latest_result <- shiny::reactiveVal(NULL)

    pa_query <- shiny::debounce(shiny::reactive({
      if (is.null(input$pa_query)) "" else input$pa_query
    }), millis = 300L)
    shiny::observeEvent(pa_query(), {
      query <- trimws(pa_query())
      if (stringi::stri_length(query) < 2L ||
          !is_suggestion_source_port(
            services$active_ingredient_suggestion_source
          )) return()
      suggestions <- tryCatch(
        services$active_ingredient_suggestion_source$suggest_active_ingredients(
          query, 15L
        ),
        error = function(error) list()
      )
      shiny::updateSelectizeInput(
        session,
        "pa",
        choices = suggestion_choices(suggestions, input$pa),
        selected = input$pa,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    excipient_context <- shiny::debounce(shiny::reactive({
      if (is.null(input$pa)) "" else input$pa
    }), millis = 300L)
    shiny::observeEvent(excipient_context(), {
      active_ingredient <- trimws(excipient_context())
      service <- services$excipient_suggestion_service
      if (stringi::stri_length(active_ingredient) < 2L ||
          !is.list(service) ||
          !is.function(service$suggest_excipients_for_active_ingredient)) return()
      suggestions <- tryCatch(
        service$suggest_excipients_for_active_ingredient(
          active_ingredient, 15L
        ),
        error = function(error) list()
      )
      shiny::updateSelectizeInput(
        session,
        "excipiente",
        choices = suggestion_choices(suggestions, input$excipiente),
        selected = input$excipiente,
        server = TRUE
      )
    }, ignoreInit = FALSE)

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
        services$search_service$search_excipient(
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
        options = list(pageLength = 15L, autoWidth = TRUE, scrollX = TRUE)
      )
    })

    render_result_group <- function(conclusion) {
      DT::renderDT({
        result <- latest_result()
        shiny::req(result)
        table <- present_grouped_search_table(result, conclusion)
        shiny::req(nrow(table) > 0L)
        DT::datatable(
          table,
          rownames = FALSE,
          escape = excifinder_dt_escape_columns(table),
          options = list(pageLength = 15L, autoWidth = TRUE, scrollX = TRUE)
        )
      })
    }
    output$results_identified <- render_result_group("identified")
    output$results_not_identified <- render_result_group("not_identified")
    output$results_indeterminate <- render_result_group("indeterminate")
    output$results_conflicting <- render_result_group("conflicting")

    output$result_groups <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      groups <- split_search_results_by_conclusion(result)
      conclusions <- c(
        "identified", "not_identified", "indeterminate", "conflicting"
      )
      visible <- conclusions[vapply(
        groups[conclusions], length, integer(1)
      ) > 0L]
      if (length(visible) == 0L) return(NULL)
      rows <- lapply(visible, function(conclusion) {
        shiny::fluidRow(shiny::column(
          width = 12,
          excifinder_state_box(
            conclusion,
            paste0("results_", conclusion)
          )
        ))
      })
      do.call(shiny::tagList, rows)
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
