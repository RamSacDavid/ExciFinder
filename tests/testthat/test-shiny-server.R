test_that("master focus restoration waits for flush and targets a stable ID", {
  callbacks <- list()
  messages <- list()
  fake_session <- list(
    onFlushed = function(callback, once) {
      callbacks[[length(callbacks) + 1L]] <<- list(
        callback = callback,
        once = once
      )
    },
    sendCustomMessage = function(type, message) {
      messages[[length(messages) + 1L]] <<- list(
        type = type,
        message = message
      )
    }
  )

  expect_false(application_env$excifinder_schedule_master_focus(
    fake_session, NULL
  ))
  expect_length(callbacks, 0L)
  expect_true(application_env$excifinder_schedule_master_focus(
    fake_session, "AUTH:ui-002"
  ))
  expect_length(messages, 0L)
  expect_true(callbacks[[1L]]$once)

  callbacks[[1L]]$callback()

  expect_identical(messages, list(list(
    type = "excifinder-focus-master-product",
    message = list(product_id = "AUTH:ui-002")
  )))
})

test_that("server calls injected service once with canonical inputs and filters", {
  fake <- new_ui_fake_search_service(make_ui_search_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "paracetamol", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_identical(length(fake$calls$items), 1L)
    expect_identical(fake$calls$items[[1]]$active_ingredient, "paracetamol")
    expect_identical(fake$calls$items[[1]]$excipient_query, "lactosa")
    expect_identical(
      fake$calls$items[[1]]$filters,
      list(authorized = TRUE, marketed = TRUE)
    )
    expect_s3_class(latest_result(), "excipient_search_result")
    expect_identical(selected_product_id(), "AUTH:ui-001")
    expect_match(
      paste(as.character(output$result_browser), collapse = ""),
      "UI Product",
      fixed = TRUE
    )
  })
})

test_that("search progress events are translated without altering the result", {
  progress_events <- list(
    list(event = "products_discovered", current = 0L, total = 1L),
    list(
      event = "product_started", current = 0L, total = 1L,
      product_id = "AUTH:ui-001", product_name = "UI Product"
    ),
    list(event = "complete", current = 1L, total = 1L)
  )
  fake <- new_ui_fake_search_service(
    make_ui_search_result(),
    progress_events = progress_events
  )

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_length(fake$calls$items, 1L)
    expect_true(is.function(fake$calls$progress[[1L]]))
    expect_s3_class(latest_result(), "excipient_search_result")
    expect_identical(selected_product_id(), "AUTH:ui-001")
  })
})

test_that("active ingredient autocomplete is debounced and ignores short queries", {
  search <- new_ui_fake_search_service(make_ui_search_result())
  suggestions <- new_ui_fake_suggestion_source(c("PARACETAMOL", "PAROXETINA"))

  shiny::testServer(application_env$build_excifinder_server(
    search_service = search$service,
    active_ingredient_suggestion_source = suggestions$source
  ), {
    session$setInputs(pa = "PARACETAMOL")
    session$setInputs(pa_query = "p")
    session$elapse(350)
    session$flushReact()
    expect_length(suggestions$calls$items, 0L)

    session$setInputs(pa_query = "para")
    session$elapse(299)
    session$flushReact()
    expect_length(suggestions$calls$items, 0L)
    session$elapse(1)
    session$flushReact()
    expect_length(suggestions$calls$items, 1L)
    expect_identical(suggestions$calls$items[[1]], list(query = "para", limit = 15L))
    expect_identical(input$pa, "PARACETAMOL")
  })
})

test_that("contextual excipient autocomplete follows PA and preserves typed values", {
  search <- new_ui_fake_search_service(make_ui_search_result())
  suggestions <- new_ui_fake_excipient_suggestion_service(
    c("Lactosa", "Sacarosa")
  )

  shiny::testServer(application_env$build_excifinder_server(
    search_service = search$service,
    excipient_suggestion_service = suggestions$service
  ), {
    session$setInputs(pa = "paracetamol", excipiente = "texto libre")
    session$elapse(300)
    session$flushReact()

    expect_length(suggestions$calls$items, 1L)
    expect_identical(
      suggestions$calls$items[[1]],
      list(active_ingredient = "paracetamol", limit = 15L)
    )
    expect_identical(input$pa, "paracetamol")
    expect_identical(input$excipiente, "texto libre")
  })
})

test_that("suggestion failures do not block free-text factual search", {
  search <- new_ui_fake_search_service(make_ui_search_result())
  pa_suggestions <- new_ui_fake_suggestion_source(fail = TRUE)
  excipient_suggestions <- new_ui_fake_excipient_suggestion_service(fail = TRUE)

  shiny::testServer(application_env$build_excifinder_server(
    search_service = search$service,
    active_ingredient_suggestion_source = pa_suggestions$source,
    excipient_suggestion_service = excipient_suggestions$service
  ), {
    session$setInputs(pa_query = "manual", pa = "manual PA", excipiente = "manual excipient")
    session$elapse(300)
    session$flushReact()
    session$setInputs(buscar = 1)
    session$flushReact()

    expect_length(pa_suggestions$calls$items, 1L)
    expect_length(excipient_suggestions$calls$items, 1L)
    expect_length(search$calls$items, 1L)
    expect_identical(search$calls$items[[1]]$active_ingredient, "manual PA")
    expect_identical(search$calls$items[[1]]$excipient_query, "manual excipient")
  })
})

test_that("server renders every non-empty factual group in master-detail order", {
  fake <- new_ui_fake_search_service(make_ui_mixed_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    browser <- paste(as.character(output$result_browser), collapse = "")
    expect_match(browser, "excifinder-master-detail", fixed = TRUE)
    expect_match(browser, "IDENTIFICADO", fixed = TRUE)
    expect_match(browser, "NO IDENTIFICADO EN FUENTES VERIFICADAS", fixed = TRUE)
    expect_match(browser, "NO VERIFICABLE", fixed = TRUE)
    expect_match(browser, "FUENTES DISCORDANTES", fixed = TRUE)
    expect_identical(selected_product_id(), "AUTH:ui-001")
    expect_match(browser, "UI Product 1", fixed = TRUE)
  })
})

test_that("initial selection uses first product in first non-empty factual group", {
  result <- make_ui_mixed_result(c(
    "conflicting", "indeterminate", "identified", "identified"
  ))
  fake <- new_ui_fake_search_service(result)

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_identical(selected_product_id(), "AUTH:ui-003")
    expect_match(
      paste(as.character(output$result_browser), collapse = ""),
      "UI Product 3",
      fixed = TRUE
    )
  })
})

test_that("selection changes detail without repeating factual search", {
  fake <- new_ui_fake_search_service(make_ui_mixed_result(c(
    "identified", "identified"
  )))
  focus_requests <- character()
  original_focus_scheduler <- application_env$excifinder_schedule_master_focus
  application_env$excifinder_schedule_master_focus <- function(
      session,
      product_id) {
    focus_requests <<- c(focus_requests, product_id)
    invisible(TRUE)
  }
  on.exit({
    application_env$excifinder_schedule_master_focus <- original_focus_scheduler
  }, add = TRUE)

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()
    expect_identical(selected_product_id(), "AUTH:ui-001")
    expect_length(focus_requests, 0L)
    result_before_selection <- latest_result()

    session$setInputs(selected_product_id = "AUTH:ui-002")
    session$flushReact()

    expect_identical(selected_product_id(), "AUTH:ui-002")
    expect_identical(latest_result(), result_before_selection)
    expect_identical(length(fake$calls$items), 1L)
    expect_identical(focus_requests, "AUTH:ui-002")
    browser <- paste(as.character(output$result_browser), collapse = "")
    expect_match(browser, "UI Product 2", fixed = TRUE)
    expect_match(browser, "aria-pressed=\"true\"", fixed = TRUE)
  })
})

test_that("a new search resets selection to its first factual product", {
  first <- make_ui_mixed_result(c("identified", "identified"))
  second <- make_ui_search_result(
    registration_number = "new-001",
    product_name = "New Search Product"
  )
  fake <- new_ui_sequential_fake_search_service(list(first, second))
  focus_requests <- character()
  original_focus_scheduler <- application_env$excifinder_schedule_master_focus
  application_env$excifinder_schedule_master_focus <- function(
      session,
      product_id) {
    focus_requests <<- c(focus_requests, product_id)
    invisible(TRUE)
  }
  on.exit({
    application_env$excifinder_schedule_master_focus <- original_focus_scheduler
  }, add = TRUE)

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "first", excipiente = "lactosa", buscar = 1)
    session$flushReact()
    session$setInputs(selected_product_id = "AUTH:ui-002")
    session$flushReact()
    expect_identical(selected_product_id(), "AUTH:ui-002")
    expect_identical(focus_requests, "AUTH:ui-002")

    session$setInputs(pa = "second", buscar = 2)
    session$flushReact()

    expect_identical(selected_product_id(), "AUTH:new-001")
    expect_identical(length(fake$calls$items), 2L)
    expect_identical(focus_requests, "AUTH:ui-002")
    expect_match(
      paste(as.character(output$result_browser), collapse = ""),
      "New Search Product",
      fixed = TRUE
    )
  })
})

test_that("single medication result selects and renders normally", {
  fake <- new_ui_fake_search_service(make_ui_search_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_identical(selected_product_id(), "AUTH:ui-001")
    browser <- paste(as.character(output$result_browser), collapse = "")
    expect_match(browser, "1 medicamento evaluado", fixed = TRUE)
    expect_match(browser, "UI Product", fixed = TRUE)
  })
})

test_that("ambiguous query renders its explanatory message", {
  fake <- new_ui_fake_search_service(make_ui_ambiguous_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "alias", buscar = 1)
    session$flushReact()

    expect_identical(length(fake$calls$items), 1L)
    expect_match(
      paste(as.character(output$search_message), collapse = ""),
      "más de un concepto"
    )
    expect_identical(
      paste(as.character(output$result_browser), collapse = ""),
      ""
    )
  })
})

test_that("zero medicines renders a dedicated non-factual message", {
  fake <- new_ui_fake_search_service(make_ui_empty_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_match(
      paste(as.character(output$search_message), collapse = ""),
      "No se encontraron medicamentos autorizados"
    )
    expect_identical(
      paste(as.character(output$result_browser), collapse = ""),
      ""
    )
  })
})

test_that("invalid resolved query does not render a clinical browser", {
  fake <- new_ui_fake_search_service(make_ui_invalid_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "invalid", buscar = 1)
    session$flushReact()

    expect_match(
      paste(as.character(output$search_message), collapse = ""),
      "no es válida"
    )
    expect_identical(
      paste(as.character(output$result_browser), collapse = ""),
      ""
    )
  })
})

test_that("partial errors render warning while retaining results", {
  result <- make_ui_search_result(
    coverage = "partial",
    errors = list(make_ui_partial_error())
  )
  fake <- new_ui_fake_search_service(result)

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_match(
      paste(as.character(output$partial_warning), collapse = ""),
      "no pudieron verificarse"
    )
    expect_match(
      paste(as.character(output$partial_errors), collapse = ""),
      "Controlled source failure"
    )
    expect_match(
      paste(as.character(output$result_browser), collapse = ""),
      "excifinder-master-detail",
      fixed = TRUE
    )
  })
})

test_that("download data uses the latest result without another search", {
  fake <- new_ui_fake_search_service(make_ui_search_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()
    calls_after_search <- length(fake$calls$items)

    export <- download_data()

    expect_identical(nrow(export), 1L)
    expect_named(export, c(
      "Medicamento", "Forma_farmaceutica", "Dosis_strength",
      "Numero_registro", "Estado", "Cobertura",
      "Metodo_busqueda", "Fuentes", "Secciones", "Evidencias",
      "URL_Ficha_Tecnica"
    ))
    expect_identical(length(fake$calls$items), calls_after_search)
  })
})

test_that("server and UI no longer expose a medicine limit input", {
  server_text <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_server.R"),
    warn = FALSE
  ), collapse = "\n")
  ui_text <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_ui.R"),
    warn = FALSE
  ), collapse = "\n")

  expect_false(grepl("input\\$limite", server_text))
  expect_false(grepl("numericInput\\(\"limite\"", ui_text))
  expect_false(grepl("No contiene", ui_text, ignore.case = TRUE))
})
