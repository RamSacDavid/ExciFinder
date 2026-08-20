test_that("presenter maps every factual and coverage state explicitly", {
  expect_identical(
    unname(vapply(
      c("identified", "not_identified", "indeterminate", "conflicting"),
      application_env$excifinder_status_label,
      character(1)
    )),
    c(
      "Identificado",
      "No identificado en fuentes verificadas",
      "No verificable",
      "Fuentes discordantes"
    )
  )
  expect_identical(
    unname(vapply(
      c("complete", "partial", "failed", "not_attempted"),
      application_env$excifinder_coverage_label,
      character(1)
    )),
    c("Completa", "Parcial", "Fallida", "No realizada")
  )
})

test_that("presenter splits original product results by conclusion in stable order", {
  conclusions <- c(
    "identified", "not_identified", "identified",
    "indeterminate", "conflicting"
  )
  singles <- lapply(seq_along(conclusions), function(index) {
    make_ui_search_result(
      conclusion = conclusions[[index]],
      product_name = paste("Product", index)
    )
  })
  mixed <- singles[[1]]
  mixed$results <- lapply(singles, function(result) result$results[[1]])

  groups <- application_env$split_search_results_by_conclusion(mixed)

  expect_named(groups, c(
    "identified", "not_identified", "indeterminate", "conflicting"
  ))
  expect_identical(
    vapply(groups$identified, function(item) item$product$name, character(1)),
    c("Product 1", "Product 3")
  )
  expect_true(all(vapply(
    unlist(groups, recursive = FALSE),
    inherits,
    logical(1),
    "product_excipient_result"
  )))
})

test_that("state classes and UI CSS preserve four explicit color identities", {
  statuses <- c("identified", "not_identified", "indeterminate", "conflicting")
  expect_identical(
    unname(vapply(statuses, application_env$excifinder_state_class, character(1))),
    c(
      "state-identified", "state-not-identified",
      "state-indeterminate", "state-conflicting"
    )
  )
  ui_html <- paste(as.character(application_env$build_excifinder_ui()), collapse = "")
  css_source <- paste(readLines(
    file.path(project_root(), "www", "excifinder.css"),
    warn = FALSE,
    encoding = "UTF-8"
  ), collapse = "\n")
  expect_true(all(vapply(statuses, function(status) {
    grepl(application_env$excifinder_state_class(status), css_source, fixed = TRUE)
  }, logical(1))))
  group_headers <- paste(vapply(statuses, function(status) {
    as.character(application_env$excifinder_state_box(status, paste0("test_", status)))
  }, character(1)), collapse = "")
  expect_match(group_headers, "IDENTIFICADO", fixed = TRUE)
  expect_match(group_headers, "NO IDENTIFICADO EN FUENTES VERIFICADAS", fixed = TRUE)
  expect_match(group_headers, "NO VERIFICABLE", fixed = TRUE)
  expect_match(group_headers, "FUENTES DISCORDANTES", fixed = TRUE)
  expect_match(ui_html, "pueden no ser exhaustivas", fixed = TRUE)
  expect_match(ui_html, '"create":true', fixed = TRUE)
  expect_match(
    ui_html,
    "No identificado en fuentes verificadas” no debe interpretarse como garantía absoluta",
    fixed = TRUE
  )
})

test_that("result table keeps state, coverage, evidence, sources, and one row per product", {
  conclusions <- c("identified", "not_identified", "indeterminate", "conflicting")
  results <- lapply(conclusions, function(conclusion) {
    make_ui_search_result(
      conclusion = conclusion,
      coverage = if (identical(conclusion, "identified")) "partial" else "complete",
      evidence_excerpts = if (conclusion %in% c("identified", "conflicting")) {
        "Original <unmodified> excerpt"
      } else {
        character()
      },
      include_structured = TRUE
    )
  })
  tables <- lapply(results, application_env$present_search_table)

  expect_true(all(vapply(tables, nrow, integer(1)) == 1L))
  expect_identical(
    vapply(tables, function(table) table$Estado[[1]], character(1)),
    c(
      "Identificado", "No identificado en fuentes verificadas",
      "No verificable", "Fuentes discordantes"
    )
  )
  expect_identical(tables[[1]]$Cobertura[[1]], "Parcial")
  expect_identical(tables[[1]]$`Forma farmacéutica`[[1]], "Comprimido")
  expect_identical(tables[[1]]$`Dosis / strength`[[1]], "500 mg")
  expect_match(tables[[1]]$Fuente[[1]], "CIMA estructurado")
  expect_match(tables[[1]]$Fuente[[1]], "Ficha técnica")
  expect_match(tables[[1]]$Evidencia[[1]], "Original <unmodified> excerpt", fixed = TRUE)
  expect_false(any(grepl("No contiene|Seguro|Libre de|Compatible", unlist(tables))))
})

test_that("parameterized tables contain only the requested factual group", {
  identified <- make_ui_search_result(conclusion = "identified")
  not_identified <- make_ui_search_result(conclusion = "not_identified")
  indeterminate <- make_ui_search_result(conclusion = "indeterminate")
  conflicting <- make_ui_search_result(conclusion = "conflicting")
  mixed <- identified
  mixed$results <- lapply(
    list(identified, not_identified, indeterminate, conflicting),
    function(result) result$results[[1]]
  )

  for (conclusion in c(
      "identified", "not_identified", "indeterminate", "conflicting")) {
    table <- application_env$present_search_table(mixed, conclusion)
    expect_identical(nrow(table), 1L)
    expect_identical(
      table$Estado[[1]], application_env$excifinder_status_label(conclusion)
    )
    expect_identical(table$Cobertura[[1]], "Completa")
    expect_match(table$Evidencia[[1]], "Original lactose excerpt", fixed = TRUE)
    expect_match(table$`Ficha técnica`[[1]], "https://example.test/document.pdf", fixed = TRUE)

    grouped <- application_env$present_grouped_search_table(mixed, conclusion)
    expect_false("Estado" %in% names(grouped))
    expect_named(grouped, c(
      "Cobertura", "Medicamento", "Forma farmacéutica", "Dosis / strength",
      "N.º registro", "Fuente", "Evidencia", "Ficha técnica"
    ))
    expect_identical(nrow(grouped), 1L)
  }
})

test_that("SmPC links are controlled, escaped, and support several candidates", {
  result <- make_ui_search_result(
    smpc_urls = c(
      "https://example.test/one.pdf?x=1&y=2",
      "https://example.test/two.pdf"
    )
  )
  link_html <- application_env$present_search_table(result)$`Ficha técnica`[[1]]

  expect_match(link_html, "Ficha técnica 1", fixed = TRUE)
  expect_match(link_html, "Ficha técnica 2", fixed = TRUE)
  expect_match(link_html, "target=\"_blank\"", fixed = TRUE)
  expect_match(link_html, "rel=\"noopener noreferrer\"", fixed = TRUE)
  expect_match(link_html, "&amp;", fixed = TRUE)
})

test_that("resolution and empty-result messages are semantically explicit", {
  literal <- application_env$present_search_messages(make_ui_search_result())
  taxonomy <- application_env$present_search_messages(make_ui_search_result(strategy = "taxonomy"))
  ambiguous <- application_env$present_search_messages(make_ui_ambiguous_result())
  invalid <- application_env$present_search_messages(make_ui_invalid_result())
  empty <- application_env$present_search_messages(make_ui_empty_result())

  expect_identical(
    literal$method,
    "Método de búsqueda: coincidencia literal normalizada"
  )
  expect_identical(
    taxonomy$method,
    "Método de búsqueda: términos controlados"
  )
  expect_match(ambiguous$primary, "más de un concepto")
  expect_match(ambiguous$primary, "Concepto A")
  expect_match(invalid$primary, "no es válida")
  expect_match(empty$primary, "No se encontraron medicamentos autorizados")
  expect_false(grepl("excipiente", empty$primary, ignore.case = TRUE))
})

test_that("partial errors remain visible without removing product rows", {
  error <- make_ui_partial_error()
  result <- make_ui_search_result(
    coverage = "partial",
    errors = list(error)
  )
  messages <- application_env$present_search_messages(result)
  errors <- application_env$present_partial_errors(result)
  table <- application_env$present_search_table(result)

  expect_identical(
    messages$warning,
    "Algunos medicamentos o fuentes no pudieron verificarse completamente."
  )
  expect_identical(nrow(table), 1L)
  expect_identical(nrow(errors), 1L)
  expect_identical(errors$Etapa[[1]], "get_source_content")
  expect_identical(errors$Mensaje[[1]], "Controlled source failure")
})

test_that("export preserves complete evidence and audit columns", {
  result <- make_ui_search_result(
    evidence_excerpts = c("First full excerpt", "Second full excerpt"),
    smpc_urls = c(
      "https://example.test/one.pdf",
      "https://example.test/two.pdf"
    )
  )
  export <- application_env$present_search_export(result)

  expect_named(export, c(
    "Medicamento", "Forma_farmaceutica", "Dosis_strength",
    "Numero_registro", "Estado", "Cobertura",
    "Metodo_busqueda", "Fuentes", "Secciones", "Evidencias",
    "URL_Ficha_Tecnica"
  ))
  expect_identical(export$Forma_farmaceutica[[1]], "Comprimido")
  expect_identical(export$Dosis_strength[[1]], "500 mg")
  expect_match(export$Evidencias[[1]], "First full excerpt", fixed = TRUE)
  expect_match(export$Evidencias[[1]], "Second full excerpt", fixed = TRUE)
  expect_match(export$Evidencias[[1]], " ||| ", fixed = TRUE)
  expect_identical(export$Secciones[[1]], "6.1")
  expect_match(export$URL_Ficha_Tecnica[[1]], "one.pdf", fixed = TRUE)
  expect_match(export$URL_Ficha_Tecnica[[1]], "two.pdf", fixed = TRUE)
})
