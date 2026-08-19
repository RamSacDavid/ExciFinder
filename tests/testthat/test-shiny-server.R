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
      "Medicamento", "Numero_registro", "Estado", "Cobertura",
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
