test_that("search inputs enforce empty, length, and control boundaries", {
  max_chars <- application_env$search_input_max_chars()

  expect_no_error(application_env$validate_search_input_text(
    paste(rep("a", max_chars), collapse = ""),
    "El principio activo"
  ))
  expect_error(
    application_env$validate_search_input_text("", "El principio activo"),
    "no puede estar vacío"
  )
  expect_error(
    application_env$validate_search_input_text(
      paste(rep("a", max_chars + 1L), collapse = ""),
      "El principio activo"
    ),
    "200 caracteres"
  )
  expect_error(
    application_env$validate_search_input_text("lacto\nsa", "El excipiente"),
    "caracteres de control"
  )
})

test_that("application rejects invalid active ingredients before source access", {
  sources <- new_search_fake_sources()
  service <- make_search_service(sources)

  expect_error(
    service$search_excipient(
      paste(rep("a", application_env$search_input_max_chars() + 1L), collapse = ""),
      "lactosa"
    ),
    "200 caracteres"
  )
  expect_error(
    service$search_excipient("para\nacetamol", "lactosa"),
    "caracteres de control"
  )
  expect_identical(search_call_count(sources), 0L)
})

test_that("Shiny text inputs expose the same 200-character boundary", {
  ui_html <- paste(as.character(application_env$build_excifinder_ui()), collapse = "")
  matches <- gregexpr('maxlength="200"', ui_html, fixed = TRUE)[[1]]

  expect_identical(sum(matches > 0L), 2L)
})

test_that("invalid Shiny inputs never call the search service", {
  fake <- new_ui_fake_search_service(make_ui_search_result())

  shiny::testServer(application_env$build_excifinder_server(fake$service), {
    session$setInputs(buscar = 0)
    session$flushReact()
    session$setInputs(pa = "", excipiente = "lactosa", buscar = 1)
    expect_silent(session$flushReact())

    expect_length(fake$calls$items, 0L)
  })
})

test_that("Excel text neutralization covers every formula prefix", {
  values <- c("=1+1", "+cmd", "-2+3", "@SUM(A1:A2)", " safe", "text", NA)
  safe <- application_env$excifinder_excel_safe_text(values)

  expect_identical(safe[1:4], paste0("'", values[1:4]))
  expect_identical(safe[5:6], values[5:6])
  expect_true(is.na(safe[[7]]))

  data <- data.frame(
    A = c("=formula", "normal"),
    B = c("+formula", "@formula"),
    stringsAsFactors = FALSE
  )
  protected <- application_env$.presenter_excel_safe_data(data)
  expect_false(any(vapply(protected, function(column) {
    any(grepl("^[=+@-]", column))
  }, logical(1))))

  result <- make_ui_search_result(product_name = "=formula")
  expect_identical(
    application_env$present_search_table(result)$Medicamento[[1]],
    "=formula"
  )
  expect_identical(
    application_env$present_search_export(result)$Medicamento[[1]],
    "'=formula"
  )
})

test_that("Excel filenames are portable, bounded, and Unicode-safe", {
  unsafe <- paste0('para/\\:*?"<>|', "   ", "café")
  filename <- application_env$excifinder_safe_filename(unsafe)

  expect_identical(filename, "ExciFinder_para_café.xlsx")
  expect_false(grepl('[/\\\\:*?"<>|]', filename))
  expect_identical(
    application_env$excifinder_safe_filename("   "),
    "ExciFinder_busqueda.xlsx"
  )
  expect_identical(
    application_env$excifinder_safe_filename("CON"),
    "ExciFinder_busqueda.xlsx"
  )
  long <- application_env$excifinder_safe_filename(
    paste(rep("á", 200L), collapse = "")
  )
  stem <- sub("[.]xlsx$", "", sub("^ExciFinder_", "", long))
  expect_lte(stringi::stri_length(stem), 80L)
})

test_that("normal result content is escaped while controlled links remain HTML", {
  malicious <- '<script>alert("x")</script>\'<'
  result <- make_ui_search_result(
    product_name = malicious,
    evidence_excerpts = malicious
  )
  table <- application_env$present_search_table(result)
  escape_columns <- application_env$excifinder_dt_escape_columns(table)
  escaped <- DT:::escapeData(table, escape_columns, names(table))

  expect_setequal(
    escape_columns,
    setdiff(names(table), "Ficha técnica")
  )
  expect_match(escaped$Medicamento[[1]], "&lt;script&gt;", fixed = TRUE)
  expect_match(escaped$Evidencia[[1]], 'alert("x")', fixed = TRUE)
  expect_false(grepl("<script>", escaped$Medicamento[[1]], fixed = TRUE))
  expect_match(table$`Ficha técnica`[[1]], "<a ", fixed = TRUE)
  expect_match(table$`Ficha técnica`[[1]], "noopener noreferrer", fixed = TRUE)
})
