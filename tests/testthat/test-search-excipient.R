test_that("application result DTOs enforce their contracts", {
  product <- make_search_product("dto", "DTO Product")
  formulation <- make_search_formulation(product)
  assessment <- application_env$assess_excipient_from_retrieved_sources(
    subject_id = domain_env$medicinal_product_id(product),
    excipient = make_factual_excipient(),
    taxonomy_version = "taxonomy-dto",
    matcher_version = "matcher-dto"
  )
  product_result <- application_env$new_product_excipient_result(
    product,
    list(formulation),
    list(),
    assessment
  )
  resolution <- application_env$new_excipient_resolution(
    "lactosa",
    "resolved",
    list(make_factual_excipient())
  )
  search_result <- application_env$new_excipient_search_result(
    list(excipient = "lactosa"),
    resolution,
    list(product_result)
  )

  expect_s3_class(product_result, "product_excipient_result")
  expect_s3_class(product_result, "excifinder_application_dto")
  expect_named(
    product_result,
    c("product", "formulations", "presentations", "assessment")
  )
  expect_s3_class(search_result, "excipient_search_result")
  expect_named(search_result, c("query", "resolution", "results", "errors"))
  expect_error(
    application_env$new_product_excipient_result(
      product,
      list("invalid"),
      list(),
      assessment
    ),
    "formulation"
  )
  expect_error(
    application_env$new_excipient_search_result(
      "query",
      resolution,
      list("invalid")
    ),
    "product_excipient_result"
  )
})

test_that("search service constructor validates every dependency", {
  sources <- new_search_fake_sources()
  service <- make_search_service(sources)

  expect_s3_class(service, "excipient_search_service")
  expect_named(service, "search_excipient")
  expect_error(
    application_env$new_excipient_search_service(
      list(),
      sources$composition_source,
      sources$artifact_source,
      make_search_taxonomy(),
      "matcher-1"
    ),
    "product_source_port"
  )
  expect_error(
    application_env$new_excipient_search_service(
      sources$product_source,
      sources$composition_source,
      sources$artifact_source,
      make_search_taxonomy(),
      ""
    ),
    "matcher_version"
  )
  expect_error(
    application_env$new_excipient_search_service(
      sources$product_source,
      sources$composition_source,
      sources$artifact_source,
      make_search_taxonomy(),
      "matcher-1",
      allow_literal_fallback = NA
    ),
    "allow_literal_fallback"
  )
})

test_that("ambiguous and strict not-found queries perform zero source calls", {
  ambiguous_taxonomy <- make_search_taxonomy(list(
    domain_env$new_excipient("excipient-a", "Alpha", synonyms = "comun"),
    domain_env$new_excipient("excipient-b", "Beta", synonyms = "comun")
  ))
  ambiguous_sources <- new_search_fake_sources()
  ambiguous <- make_search_service(
    ambiguous_sources,
    ambiguous_taxonomy
  )$search_excipient("ingredient", "comun")

  not_found_sources <- new_search_fake_sources()
  not_found <- make_search_service(
    not_found_sources,
    allow_literal_fallback = FALSE
  )$search_excipient(
    "ingredient",
    "desconocido"
  )

  expect_identical(ambiguous$resolution$status, "ambiguous")
  expect_length(ambiguous$results, 0L)
  expect_identical(search_call_count(ambiguous_sources), 0L)
  expect_identical(not_found$resolution$status, "not_found")
  expect_length(not_found$results, 0L)
  expect_identical(search_call_count(not_found_sources), 0L)
})

test_that("zero discovered products is a valid empty result", {
  sources <- new_search_fake_sources(products = list())
  result <- make_search_service(sources)$search_excipient(
    "ingredient",
    "lactosa"
  )

  expect_s3_class(result, "excipient_search_result")
  expect_length(result$results, 0L)
  expect_length(result$errors, 0L)
  expect_identical(search_call_count(sources, "find_products"), 1L)
  expect_identical(search_call_count(sources), 1L)
  expect_identical(
    sources$calls$events[[1]]$filters,
    list(authorized = TRUE, marketed = TRUE)
  )
})

test_that("single-product factual outcomes preserve prior semantics", {
  cases <- list(
    A = make_single_product_sources("Lactosa", "Contiene lactosa"),
    B = make_single_product_sources("Sacarosa", "Contiene sacarosa"),
    C = make_single_product_sources("Sacarosa"),
    D = make_single_product_sources(
      "Lactosa",
      document_value = make_search_source_failure("document failed")
    )
  )
  results <- lapply(cases, function(case) {
    case$service$search_excipient("ingredient", "lactosa")
  })
  assessments <- lapply(results, function(result) result$results[[1]]$assessment)

  expect_identical(
    vapply(assessments, `[[`, character(1), "factual_conclusion"),
    c(A = "identified", B = "not_identified", C = "indeterminate", D = "identified")
  )
  expect_identical(
    vapply(assessments, `[[`, character(1), "verification_coverage"),
    c(A = "complete", B = "complete", C = "partial", D = "partial")
  )
  expect_length(results$D$errors, 1L)
  expect_identical(results$D$errors[[1]]$stage, "get_source_content")
})

test_that("multiple formulations aggregate structured evidence without synthetic snapshots", {
  product <- make_search_product("multi", "Multiple Formulations")
  product_id <- domain_env$medicinal_product_id(product)
  formulation_a <- make_search_formulation(product, "a")
  formulation_b <- make_search_formulation(product, "b")
  snapshot_a <- make_search_snapshot(formulation_a, "Sacarosa", "a")
  snapshot_b <- make_search_snapshot(formulation_b, "Lactosa", "b")
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation_b, formulation_a)), product_id),
    snapshots = setNames(
      list(snapshot_a, snapshot_b),
      c(formulation_a$id, formulation_b$id)
    )
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")
  assessment <- result$results[[1]]$assessment
  structured_attempts <- Filter(function(attempt) {
    identical(attempt$method, "structured_composition_controlled_terms")
  }, assessment$attempts)

  expect_identical(assessment$factual_conclusion, "identified")
  expect_identical(assessment$verification_coverage, "partial")
  expect_length(structured_attempts, 2L)
  expect_identical(
    vapply(structured_attempts, `[[`, character(1), "outcome"),
    c("no_evidence", "evidence_found")
  )
  expect_identical(
    vapply(result$results[[1]]$formulations, `[[`, character(1), "id"),
    sort(c(formulation_a$id, formulation_b$id))
  )
})

test_that("multiple structured negatives still require valid document absence", {
  product <- make_search_product("negative", "Negative Product")
  product_id <- domain_env$medicinal_product_id(product)
  formulations <- list(
    make_search_formulation(product, "1"),
    make_search_formulation(product, "2")
  )
  snapshots <- setNames(lapply(seq_along(formulations), function(index) {
    make_search_snapshot(formulations[[index]], "Sacarosa", as.character(index))
  }), vapply(formulations, `[[`, character(1), "id"))
  document <- make_search_document(product)
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(formulations), product_id),
    snapshots = snapshots,
    artifacts = setNames(list(list(document)), product_id),
    contents = setNames(list(make_search_content(document, "Contiene sacarosa")), document$id)
  )
  assessment <- make_search_service(sources)$search_excipient(
    "ingredient",
    "lactosa"
  )$results[[1]]$assessment

  expect_identical(assessment$factual_conclusion, "not_identified")
  expect_identical(assessment$verification_coverage, "complete")
})

test_that("structured failures coexist with usable positive snapshots", {
  product <- make_search_product("partial", "Partial Product")
  product_id <- domain_env$medicinal_product_id(product)
  formulation_a <- make_search_formulation(product, "a")
  formulation_b <- make_search_formulation(product, "b")
  snapshot_a <- make_search_snapshot(formulation_a, "Lactosa")
  source_failure <- make_search_source_failure("composition timeout")
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation_a, formulation_b)), product_id),
    snapshots = setNames(
      list(snapshot_a, source_failure),
      c(formulation_a$id, formulation_b$id)
    )
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")
  assessment <- result$results[[1]]$assessment

  expect_identical(assessment$factual_conclusion, "identified")
  expect_identical(assessment$verification_coverage, "partial")
  expect_true(any(vapply(assessment$attempts, function(attempt) {
    identical(attempt$extraction_status, "failed") && !is.null(attempt$error)
  }, logical(1))))
  expect_true(any(vapply(result$errors, function(error) {
    identical(error$stage, "get_composition_snapshot") &&
      identical(error$subject_id, formulation_b$id)
  }, logical(1))))
})

test_that("multiple-snapshot conflict remains asymmetric", {
  product <- make_search_product("conflict", "Conflict Product")
  product_id <- domain_env$medicinal_product_id(product)
  formulations <- list(
    make_search_formulation(product, "1"),
    make_search_formulation(product, "2")
  )
  document <- make_search_document(product)
  run_case <- function(entry_names, document_text) {
    snapshots <- setNames(lapply(seq_along(formulations), function(index) {
      make_search_snapshot(
        formulations[[index]],
        entry_names[[index]],
        as.character(index)
      )
    }), vapply(formulations, `[[`, character(1), "id"))
    sources <- new_search_fake_sources(
      products = list(product),
      details = setNames(list(product), product_id),
      formulations = setNames(list(formulations), product_id),
      snapshots = snapshots,
      artifacts = setNames(list(list(document)), product_id),
      contents = setNames(list(make_search_content(document, document_text)), document$id)
    )
    make_search_service(sources)$search_excipient(
      "ingredient",
      "lactosa"
    )$results[[1]]$assessment$factual_conclusion
  }

  expect_identical(run_case(list("Lactosa", "Sacarosa"), "Sacarosa"), "conflicting")
  expect_identical(run_case(list("Sacarosa", "Almidon"), "Lactosa"), "identified")
})

test_that("missing product detail is partial and other products continue", {
  product_a <- make_search_product("a", "Alpha Product")
  product_b <- make_search_product("b", "Broken Product")
  product_c <- make_search_product("c", "Charlie Product")
  id_a <- domain_env$medicinal_product_id(product_a)
  id_b <- domain_env$medicinal_product_id(product_b)
  id_c <- domain_env$medicinal_product_id(product_c)
  sources <- new_search_fake_sources(
    products = list(product_c, product_b, product_a),
    details = setNames(
      list(product_a, application_env$new_port_absent(), product_c),
      c(id_a, id_b, id_c)
    )
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")

  expect_identical(
    vapply(result$results, function(item) item$product$name, character(1)),
    c("Alpha Product", "Charlie Product")
  )
  expect_length(result$errors, 1L)
  expect_identical(result$errors[[1]]$stage, "get_product")
  expect_identical(result$errors[[1]]$subject_id, id_b)
  expect_false(any(vapply(result$results, function(item) {
    identical(domain_env$medicinal_product_id(item$product), id_b)
  }, logical(1))))
})

test_that("latest uniquely dated document is selected", {
  product <- make_search_product("latest", "Latest Document")
  product_id <- domain_env$medicinal_product_id(product)
  formulation <- make_search_formulation(product)
  snapshot <- make_search_snapshot(formulation, "Sacarosa")
  older <- make_search_document(
    product,
    "older",
    as.POSIXct("2025-01-01", tz = "UTC")
  )
  latest <- make_search_document(
    product,
    "latest",
    as.POSIXct("2026-01-01", tz = "UTC")
  )
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation)), product_id),
    snapshots = setNames(list(snapshot), formulation$id),
    artifacts = setNames(list(list(older, latest)), product_id),
    contents = setNames(
      list(
        make_search_content(older, "Lactosa"),
        make_search_content(latest, "Sacarosa")
      ),
      c(older$id, latest$id)
    )
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")
  content_calls <- Filter(function(event) {
    identical(event$operation, "get_source_content")
  }, sources$calls$events)

  expect_identical(result$results[[1]]$assessment$factual_conclusion, "not_identified")
  expect_length(content_calls, 1L)
  expect_identical(content_calls[[1]]$subject_id, latest$id)
})

test_that("ambiguous document metadata is not selected arbitrarily", {
  product <- make_search_product("ambiguous-doc", "Ambiguous Document")
  product_id <- domain_env$medicinal_product_id(product)
  formulation <- make_search_formulation(product)
  snapshot <- make_search_snapshot(formulation, "Sacarosa")
  same_date <- as.POSIXct("2026-01-01", tz = "UTC")
  first <- make_search_document(product, "a", same_date)
  second <- make_search_document(product, "b", same_date)
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation)), product_id),
    snapshots = setNames(list(snapshot), formulation$id),
    artifacts = setNames(list(list(first, second)), product_id)
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")

  expect_identical(result$results[[1]]$assessment$factual_conclusion, "indeterminate")
  expect_identical(result$results[[1]]$assessment$verification_coverage, "partial")
  expect_identical(search_call_count(sources, "get_source_content"), 0L)
  expect_true(any(vapply(result$errors, function(error) {
    identical(error$code, "ambiguous_source_artifact")
  }, logical(1))))
})

test_that("presentations are deduplicated by identity and ordered stably", {
  product <- make_search_product("presentations", "Presentation Product")
  product_id <- domain_env$medicinal_product_id(product)
  formulation_a <- make_search_formulation(product, "a")
  formulation_b <- make_search_formulation(product, "b")
  duplicate_a <- make_search_presentation(formulation_a, "200", "Zulu")
  duplicate_b <- make_search_presentation(formulation_b, "200", "Duplicate")
  first <- make_search_presentation(formulation_a, "100", "Alpha")
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation_b, formulation_a)), product_id),
    presentations = setNames(
      list(list(first, duplicate_a), list(duplicate_b)),
      c(formulation_a$id, formulation_b$id)
    )
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")
  presentations <- result$results[[1]]$presentations

  expect_length(presentations, 2L)
  expect_identical(
    vapply(presentations, `[[`, character(1), "description"),
    c("Alpha", "Zulu")
  )
  expect_length(unique(vapply(
    presentations,
    domain_env$presentation_id,
    character(1)
  )), 2L)
})

test_that("product results are alphabetically stable and contain one product assessment", {
  products <- list(
    make_search_product("z", "Zulu"),
    make_search_product("a", "alpha"),
    make_search_product("b", "Beta")
  )
  ids <- vapply(products, domain_env$medicinal_product_id, character(1))
  sources <- new_search_fake_sources(
    products = products,
    details = setNames(products, ids)
  )
  result <- make_search_service(sources)$search_excipient("ingredient", "lactosa")

  expect_identical(
    vapply(result$results, function(item) item$product$name, character(1)),
    c("alpha", "Beta", "Zulu")
  )
  expect_true(all(vapply(result$results, function(item) {
    inherits(item$assessment, "excipient_assessment") &&
      identical(
        item$assessment$subject_id,
        domain_env$medicinal_product_id(item$product)
      )
  }, logical(1))))
})
