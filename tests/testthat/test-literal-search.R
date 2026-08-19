test_that("taxonomy canonical and synonym resolutions retain taxonomy strategy", {
  taxonomy <- make_search_taxonomy(list(domain_env$new_excipient(
    "excipient-lactose",
    "lactosa",
    synonyms = "azúcar de leche"
  )))
  canonical_sources <- new_search_fake_sources()
  synonym_sources <- new_search_fake_sources()

  canonical <- make_search_service(
    canonical_sources,
    taxonomy
  )$search_excipient("ingredient", "lactosa")
  synonym <- make_search_service(
    synonym_sources,
    taxonomy
  )$search_excipient("ingredient", "azucar de leche")

  expect_identical(canonical$resolution$status, "resolved")
  expect_identical(canonical$resolution$strategy, "taxonomy")
  expect_identical(synonym$resolution$status, "resolved")
  expect_identical(synonym$resolution$strategy, "taxonomy")
  expect_identical(
    synonym$resolution$matched_terms[[1]]$sources,
    "synonym"
  )
  expect_identical(search_call_count(canonical_sources, "find_products"), 1L)
  expect_identical(search_call_count(synonym_sources, "find_products"), 1L)
})

test_that("not-found taxonomy query falls back to literal search by default", {
  fixture <- make_single_product_sources("Crospovidona tipo A")
  result <- fixture$service$search_excipient("ingredient", "Crospovidona")
  assessment <- result$results[[1]]$assessment

  expect_identical(result$resolution$status, "resolved")
  expect_identical(result$resolution$strategy, "literal")
  expect_true(startsWith(result$resolution$candidates[[1]]$id, "literal:v1:"))
  expect_gt(search_call_count(fixture$sources), 0L)
  expect_identical(assessment$factual_conclusion, "identified")
  expect_identical(assessment$taxonomy_version, "literal-v1")
})

test_that("strict taxonomy mode keeps not-found behavior without source calls", {
  sources <- new_search_fake_sources()
  result <- make_search_service(
    sources,
    allow_literal_fallback = FALSE
  )$search_excipient("ingredient", "Crospovidona")

  expect_identical(result$resolution$status, "not_found")
  expect_identical(result$resolution$strategy, "taxonomy")
  expect_length(result$results, 0L)
  expect_identical(search_call_count(sources), 0L)
})

test_that("ambiguous taxonomy never falls back to literal mode", {
  taxonomy <- make_search_taxonomy(list(
    domain_env$new_excipient("excipient-a", "Alpha", synonyms = "shared"),
    domain_env$new_excipient("excipient-b", "Beta", synonyms = "shared")
  ))
  sources <- new_search_fake_sources()
  result <- make_search_service(sources, taxonomy)$search_excipient(
    "ingredient",
    "shared"
  )

  expect_identical(result$resolution$status, "ambiguous")
  expect_identical(result$resolution$strategy, "taxonomy")
  expect_length(result$results, 0L)
  expect_identical(search_call_count(sources), 0L)
})

test_that("invalid literal queries return application validation errors without sources", {
  cases <- list(
    empty = "",
    whitespace = "   ",
    too_long = strrep(
      "a",
      application_env$literal_excipient_query_max_chars() + 1L
    ),
    control = paste0("valid", "\u0007")
  )

  for (query_name in names(cases)) {
    sources <- new_search_fake_sources()
    result <- make_search_service(sources)$search_excipient(
      "ingredient",
      cases[[query_name]]
    )

    expect_identical(result$resolution$status, "not_found", info = query_name)
    expect_identical(result$resolution$strategy, "taxonomy", info = query_name)
    expect_identical(length(result$results), 0L, info = query_name)
    expect_identical(length(result$errors), 1L, info = query_name)
    expect_identical(
      result$errors[[1]]$code,
      "invalid_literal_query",
      info = query_name
    )
    expect_null(result$errors[[1]]$subject_id, info = query_name)
    expect_false(
      inherits(result$errors[[1]]$condition, "excifinder_port_error"),
      info = query_name
    )
    expect_identical(search_call_count(sources), 0L, info = query_name)
  }
})

test_that("taxonomic assessments preserve the real taxonomy version", {
  fixture <- make_single_product_sources("Lactosa")
  result <- fixture$service$search_excipient("ingredient", "lactosa")

  expect_identical(result$resolution$strategy, "taxonomy")
  expect_identical(
    result$results[[1]]$assessment$taxonomy_version,
    "taxonomy-search-1"
  )
})

test_that("literal service behavior remains source-implementation independent", {
  fixture <- make_single_product_sources("A+B? (tipo 1)")
  result <- fixture$service$search_excipient(
    "ingredient",
    "A+B? (tipo 1)"
  )

  expect_s3_class(result, "excipient_search_result")
  expect_identical(result$resolution$strategy, "literal")
  expect_identical(result$results[[1]]$assessment$factual_conclusion, "identified")
  expect_true(all(vapply(fixture$sources$calls$events, function(event) {
    event$operation %in% c(
      "find_products", "get_product", "get_formulations",
      "get_presentations", "get_composition_snapshot",
      "list_source_artifacts", "get_source_content"
    )
  }, logical(1))))
})
