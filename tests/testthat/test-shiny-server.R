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
    expect_silent(output$results_table)
    expect_identical(
      application_env$present_search_table(latest_result())$Estado[[1]],
      "Identificado"
    )
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

test_that("server renders every factual result in its separate group", {
  fake <- new_ui_fake_search_service(make_ui_mixed_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
    session$flushReact()

    expect_silent(output$results_identified)
    expect_silent(output$results_not_identified)
    expect_silent(output$results_indeterminate)
    expect_silent(output$results_conflicting)
    groups <- paste(as.character(output$result_groups), collapse = "")
    expect_match(groups, "IDENTIFICADO", fixed = TRUE)
    expect_match(groups, "NO IDENTIFICADO EN FUENTES VERIFICADAS", fixed = TRUE)
    expect_match(groups, "NO VERIFICABLE", fixed = TRUE)
    expect_match(groups, "FUENTES DISCORDANTES", fixed = TRUE)
    expect_identical(lengths(regmatches(
      groups,
      gregexpr("col-sm-12", groups, fixed = TRUE)
    )), 4L)
    expect_false(grepl("col-sm-6", groups, fixed = TRUE))
  })
})

test_that("each factual state renders only its own full-width panel", {
  titles <- c(
    identified = "IDENTIFICADO",
    not_identified = "NO IDENTIFICADO EN FUENTES VERIFICADAS",
    indeterminate = "NO VERIFICABLE",
    conflicting = "FUENTES DISCORDANTES"
  )

  for (conclusion in names(titles)) {
    fake <- new_ui_fake_search_service(make_ui_mixed_result(conclusion))
    shiny::testServer(application_env$build_excifinder_server(fake$service), {
      session$setInputs(buscar = 0)
      session$flushReact()
      session$setInputs(pa = "ingredient", excipiente = "lactosa", buscar = 1)
      session$flushReact()

      groups <- paste(as.character(output$result_groups), collapse = "")
      expect_match(groups, titles[[conclusion]], fixed = TRUE)
      expect_match(
        groups,
        application_env$excifinder_state_class(conclusion),
        fixed = TRUE
      )
      expect_identical(lengths(regmatches(
        groups,
        gregexpr("excifinder-state-group", groups, fixed = TRUE)
      )), 1L)
      expect_match(groups, "col-sm-12", fixed = TRUE)
      expect_false(grepl("col-sm-6", groups, fixed = TRUE))
      for (other in setdiff(names(titles), conclusion)) {
        expect_false(grepl(
          application_env$excifinder_state_class(other),
          groups,
          fixed = TRUE
        ))
      }
    })
  }
})

test_that("group layout stays vertical, full-width, and scroll-protected", {
  server_text <- paste(readLines(
    file.path(project_root(), "R", "ui", "shiny_server.R"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")
  css_text <- paste(readLines(
    file.path(project_root(), "www", "excifinder.css"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")
  result <- make_ui_mixed_result(c("identified", "identified"))

  expect_identical(
    nrow(application_env$present_grouped_search_table(result, "identified")),
    2L
  )
  expect_match(server_text, "width = 12", fixed = TRUE)
  expect_false(grepl("width = 6", server_text, fixed = TRUE))
  expect_false(grepl("width = 6", css_text, fixed = TRUE))
  expect_match(server_text, "scrollX = TRUE", fixed = TRUE)
  expect_match(css_text, "overflow: hidden", fixed = TRUE)
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
    expect_error(output$results_table, class = "shiny.silent.error")
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
    expect_error(output$results_table, class = "shiny.silent.error")
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
    expect_silent(output$results_table)
    expect_identical(
      application_env$present_search_table(latest_result())$Estado[[1]],
      "Identificado"
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
