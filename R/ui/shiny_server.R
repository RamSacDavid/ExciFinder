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

excifinder_schedule_master_focus <- function(session, product_id) {
  if (!is.character(product_id) || length(product_id) != 1L ||
      is.na(product_id) || !nzchar(product_id)) {
    return(invisible(FALSE))
  }
  session$onFlushed(function() {
    session$sendCustomMessage(
      "excifinder-focus-master-product",
      list(product_id = product_id)
    )
  }, once = TRUE)
  invisible(TRUE)
}

excifinder_search_progress_handler <- function(set_progress) {
  force(set_progress)
  function(event, current, total, product_id = NULL, product_name = NULL) {
    if (identical(event, "products_discovered")) {
      detail <- if (identical(total, 0L)) {
        "No se encontraron medicamentos para verificar."
      } else {
        sprintf("%d medicamentos encontrados. Iniciando verificación.", total)
      }
      set_progress(value = 0, detail = detail)
    } else if (identical(event, "product_started") && total > 0L) {
      set_progress(
        value = current / total,
        detail = sprintf("Verificando medicamentos: %d de %d", current, total)
      )
    } else if (identical(event, "complete")) {
      set_progress(
        value = 1,
        detail = sprintf("Verificación completada: %d de %d", current, total)
      )
    }
    invisible(NULL)
  }
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
    selected_product_id <- shiny::reactiveVal(NULL)

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
      result <- shiny::withProgress(
        message = "Consultando CIMA…",
        value = 0,
        detail = "Buscando medicamentos autorizados y comercializados.",
        {
          progress <- excifinder_search_progress_handler(function(value, detail) {
            shiny::setProgress(value = value, detail = detail)
          })
          tryCatch(
        services$search_service$search_excipient(
          active_ingredient = input$pa,
          excipient_query = input$excipiente,
          filters = list(authorized = TRUE, marketed = TRUE),
          progress = progress
        ),
        error = function(error) {
          shiny::showNotification(
            "No fue posible completar la búsqueda. Inténtelo de nuevo.",
            type = "error"
          )
          NULL
        }
          )
        }
      )
      if (!is.null(result)) {
        latest_result(result)
        browser <- present_search_browser(result)
        selected_product_id(if (is.null(browser)) {
          NULL
        } else {
          browser$selected_product_id
        })
      }
    }, ignoreInit = TRUE)

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

    shiny::observeEvent(input$selected_product_id, {
      result <- latest_result()
      shiny::req(result)
      browser <- present_search_browser(result, input$selected_product_id)
      if (!is.null(browser) && identical(
          browser$selected_product_id,
          input$selected_product_id)) {
        selected_product_id(input$selected_product_id)
        excifinder_schedule_master_focus(session, input$selected_product_id)
      }
    }, ignoreInit = TRUE)

    output$result_browser <- shiny::renderUI({
      result <- latest_result()
      shiny::req(result)
      browser <- present_search_browser(result, selected_product_id())
      if (is.null(browser)) return(NULL)
      excifinder_result_browser(browser)
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
