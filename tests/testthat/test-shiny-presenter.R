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
  group_headers <- as.character(application_env$excifinder_result_browser(
    application_env$present_search_browser(make_ui_mixed_result())
  ))
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

test_that("clinical presenter maps the complete identified medication card", {
  result <- make_ui_search_result(
    conclusion = "identified",
    coverage = "partial",
    routes = c("Oral", "Sublingual"),
    active_ingredient_query = "FINERENONA",
    excipient_query = "LACTOSA MONOHIDRATO",
    evidence_excerpts = c("First <literal> excerpt", "Second literal excerpt"),
    smpc_urls = c(
      "https://example.test/one.pdf?x=1&y=2",
      "https://example.test/two.pdf"
    ),
    include_structured = TRUE
  )
  browser <- application_env$present_search_browser(result)
  detail <- browser$detail
  html <- as.character(application_env$excifinder_result_browser(browser))

  expect_identical(browser$context$active_ingredient, "FINERENONA")
  expect_identical(browser$context$excipient, "LACTOSA MONOHIDRATO")
  expect_identical(browser$context$product_count, 1L)
  expect_identical(browser$context$method, "Coincidencia literal normalizada")
  expect_identical(detail$conclusion, "identified")
  expect_identical(detail$status_label, "Identificado")
  expect_identical(detail$fields$dose, "500 mg")
  expect_identical(detail$fields$pharmaceutical_form, "Comprimido")
  expect_identical(detail$fields$administration_route, "Oral · Sublingual")
  expect_identical(detail$fields$excipient, "LACTOSA MONOHIDRATO")
  expect_identical(detail$fields$coverage, "Parcial")
  expect_match(detail$fields$sources, "CIMA estructurado", fixed = TRUE)
  expect_match(detail$fields$sources, "Ficha técnica (sección 6.1)", fixed = TRUE)
  expect_identical(vapply(detail$evidence, `[[`, character(1), "excerpt"), c(
    "First <literal> excerpt", "Second literal excerpt"
  ))
  expect_true(all(vapply(
    detail$evidence,
    function(item) identical(item$matched_term, "lactosa"),
    logical(1)
  )))
  expect_match(html, ">Dosis<", fixed = TRUE)
  expect_false(grepl("Dosis / strength", html, fixed = TRUE))
  expect_identical(lengths(regmatches(
    html,
    gregexpr('class="excifinder-clinical-field"', html, fixed = TRUE)
  )), 6L)
  for (label in c(
      "Dosis", "Forma farmacéutica", "Vía de administración",
      "Excipiente consultado", "Cobertura de verificación",
      "Fuentes verificadas"
  )) {
    expect_match(html, paste0(">", label, "<"), fixed = TRUE)
  }
  expect_match(html, "VERIFICACIÓN POR FUENTE", fixed = TRUE)
  expect_match(html, "Evidencia encontrada", fixed = TRUE)
  expect_match(html, "Sin evidencia", fixed = TRUE)
  expect_match(html, "Verificación completa", fixed = TRUE)
  expect_match(html, "Ficha técnica (sección 6.1)", fixed = TRUE)
  expect_match(html, "Ver 1 evidencia adicional", fixed = TRUE)
  expect_match(html, "First &lt;literal&gt; excerpt", fixed = TRUE)
  expect_false(grepl("First <literal> excerpt", html, fixed = TRUE))
})

test_that("clinical presenter aggregates unique formulation values safely", {
  result <- make_ui_search_result(routes = c("Oral", "Oral"))
  product_result <- result$results[[1]]
  second <- domain_env$new_formulation(
    id = paste0(product_result$assessment$subject_id, ":formulation:2"),
    medicinal_product_id = product_result$assessment$subject_id,
    pharmaceutical_form = "Cápsula",
    routes = c("Oral", "Bucal"),
    strength = "250 mg"
  )
  result$results[[1]]$formulations <- c(product_result$formulations, list(second))
  detail <- application_env$present_search_browser(result)$detail

  expect_identical(detail$fields$dose, "500 mg · 250 mg")
  expect_identical(
    detail$fields$pharmaceutical_form,
    "Comprimido · Cápsula"
  )
  expect_identical(detail$fields$administration_route, "Oral · Bucal")
})

test_that("clinical cards preserve neutral messages for non-positive states", {
  not_identified <- application_env$present_search_browser(
    make_ui_search_result(
      conclusion = "not_identified",
      evidence_excerpts = character()
    )
  )$detail
  indeterminate <- application_env$present_search_browser(
    make_ui_search_result(
      conclusion = "indeterminate",
      coverage = "failed",
      evidence_excerpts = character()
    )
  )$detail

  expect_identical(
    not_identified$no_evidence_message,
    "Sin coincidencias en fuentes verificadas."
  )
  expect_false(grepl(
    "No contiene|Ausencia confirmada|Libre de",
    not_identified$no_evidence_message,
    ignore.case = TRUE
  ))
  expect_identical(
    indeterminate$no_evidence_message,
    "No se dispone de evidencia concluyente."
  )
  expect_identical(indeterminate$fields$coverage, "Fallida")
})

test_that("conflicting card exposes discordant attempts and positive evidence", {
  detail <- application_env$present_search_browser(make_ui_search_result(
    conclusion = "conflicting",
    include_structured = TRUE,
    evidence_excerpts = c("Conflict evidence one", "Conflict evidence two")
  ))$detail

  expect_identical(detail$status_label, "Fuentes discordantes")
  expect_identical(
    vapply(detail$attempts, `[[`, character(1), "outcome"),
    c("Sin evidencia", "Evidencia encontrada")
  )
  expect_identical(
    vapply(detail$evidence, `[[`, character(1), "excerpt"),
    c("Conflict evidence one", "Conflict evidence two")
  )
})

test_that("clinical labels cover all outcomes and extraction statuses", {
  expect_identical(unname(vapply(
    c("evidence_found", "no_evidence", "inconclusive", "not_attempted"),
    application_env$excifinder_attempt_outcome_label,
    character(1)
  )), c(
    "Evidencia encontrada", "Sin evidencia", "No concluyente", "No verificado"
  ))
  expect_identical(unname(vapply(
    c("complete", "partial", "failed", "not_attempted"),
    application_env$excifinder_extraction_status_label,
    character(1)
  )), c(
    "Verificación completa", "Verificación parcial",
    "Verificación fallida", "No realizada"
  ))
})

test_that("clinical SmPC links are multiple, controlled, and safe", {
  browser <- application_env$present_search_browser(make_ui_search_result(
    smpc_urls = c(
      "https://example.test/one.pdf?x=1&y=2",
      "https://example.test/two.pdf"
    )
  ))
  links <- browser$detail$smpc_links
  html <- as.character(application_env$excifinder_result_browser(browser))

  expect_identical(length(links), 2L)
  expect_identical(vapply(links, `[[`, character(1), "label"), c(
    "Abrir ficha técnica 1", "Abrir ficha técnica 2"
  ))
  expect_match(html, "target=\"_blank\"", fixed = TRUE)
  expect_match(html, "rel=\"noopener noreferrer\"", fixed = TRUE)
  expect_match(html, "x=1&amp;y=2", fixed = TRUE)
  expect_false(grepl(">https://", html, fixed = TRUE))
})

test_that("master groups preserve factual order, counts, and stable products", {
  browser <- application_env$present_search_browser(make_ui_mixed_result(c(
    "conflicting", "identified", "identified", "indeterminate"
  )))

  expect_identical(
    vapply(browser$groups, `[[`, character(1), "conclusion"),
    c("identified", "indeterminate", "conflicting")
  )
  expect_identical(vapply(browser$groups, `[[`, integer(1), "count"), c(2L, 1L, 1L))
  expect_identical(browser$selected_product_id, "AUTH:ui-002")
  expect_identical(browser$detail$name, "UI Product 2")
})

test_that("clinical browser is absent for unresolved or empty searches", {
  expect_null(application_env$present_search_browser(make_ui_ambiguous_result()))
  expect_null(application_env$present_search_browser(make_ui_invalid_result()))
  expect_null(application_env$present_search_browser(make_ui_empty_result()))
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
