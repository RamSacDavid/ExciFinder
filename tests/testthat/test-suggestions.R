test_that("suggestion DTO and source port validate query, result, and limit", {
  calls <- list()
  port <- application_env$new_suggestion_source_port(function(query, limit) {
    calls[[length(calls) + 1L]] <<- list(query = query, limit = limit)
    list(application_env$new_suggestion("pa-1", "Paracetamol"))
  })

  result <- port$suggest_active_ingredients("para", 10L)
  expect_s3_class(port, "suggestion_source_port")
  expect_s3_class(result[[1]], "suggestion")
  expect_identical(result[[1]]$value, "Paracetamol")
  expect_identical(calls[[1]], list(query = "para", limit = 10L))
  expect_error(port$suggest_active_ingredients("", 10L), "non-empty")
  expect_error(port$suggest_active_ingredients("para", 0L), "positive integer")
})

test_that("suggestion source supports zero, multiple, limited, and failed results", {
  empty <- application_env$new_suggestion_source_port(function(query, limit) list())
  multiple <- application_env$new_suggestion_source_port(function(query, limit) {
    lapply(seq_len(limit), function(index) {
      application_env$new_suggestion(as.character(index), paste("Name", index))
    })
  })
  failed <- application_env$new_suggestion_source_port(function(query, limit) {
    stop("controlled source failure")
  })

  expect_length(empty$suggest_active_ingredients("pa", 15L), 0L)
  expect_length(multiple$suggest_active_ingredients("pa", 3L), 3L)
  expect_error(
    failed$suggest_active_ingredients("pa", 15L),
    "controlled source failure"
  )
})

test_that("contextual excipient suggestions use only structured composition", {
  product_a <- make_search_product("suggest-a", "Product A")
  product_b <- make_search_product("suggest-b", "Product B")
  product_a_id <- domain_env$medicinal_product_id(product_a)
  product_b_id <- domain_env$medicinal_product_id(product_b)
  formulation_a <- make_search_formulation(product_a)
  formulation_b <- make_search_formulation(product_b)
  snapshot_a <- make_search_snapshot(
    formulation_a,
    c("Lactósa", "Almidón de maíz", "Sacarosa")
  )
  snapshot_b <- make_search_snapshot(
    formulation_b,
    c("lactosa", "Celulosa microcristalina")
  )
  sources <- new_search_fake_sources(
    products = list(product_a, product_b),
    formulations = setNames(
      list(list(formulation_a), list(formulation_b)),
      c(product_a_id, product_b_id)
    ),
    snapshots = setNames(
      list(snapshot_a, snapshot_b),
      c(formulation_a$id, formulation_b$id)
    )
  )
  service <- application_env$new_excipient_suggestion_service(
    sources$product_source,
    sources$composition_source
  )

  suggestions <- service$suggest_excipients_for_active_ingredient("ingredient", 3L)

  expect_identical(
    vapply(suggestions, `[[`, character(1), "value"),
    c("Lactósa", "Almidón de maíz", "Sacarosa")
  )
  expect_identical(search_call_count(sources, "get_product"), 0L)
  expect_identical(search_call_count(sources, "list_source_artifacts"), 0L)
  expect_identical(search_call_count(sources, "get_source_content"), 0L)
})

test_that("contextual suggestions tolerate empty and partially failed formulations", {
  product <- make_search_product("suggest-partial", "Partial")
  product_id <- domain_env$medicinal_product_id(product)
  failed_formulation <- make_search_formulation(product, "failed")
  good_formulation <- make_search_formulation(product, "good")
  good_snapshot <- make_search_snapshot(good_formulation, "Xylitol", "good")
  sources <- new_search_fake_sources(
    products = list(product),
    formulations = setNames(
      list(list(failed_formulation, good_formulation)), product_id
    ),
    snapshots = setNames(
      list(simpleError("controlled composition failure"), good_snapshot),
      c(failed_formulation$id, good_formulation$id)
    )
  )
  service <- application_env$new_excipient_suggestion_service(
    sources$product_source,
    sources$composition_source
  )

  expect_identical(
    vapply(
      service$suggest_excipients_for_active_ingredient("ingredient"),
      `[[`, character(1), "value"
    ),
    "Xylitol"
  )

  empty_sources <- new_search_fake_sources(products = list(product))
  empty_service <- application_env$new_excipient_suggestion_service(
    empty_sources$product_source,
    empty_sources$composition_source
  )
  expect_length(
    empty_service$suggest_excipients_for_active_ingredient("ingredient"), 0L
  )
})

test_that("contextual suggestions do not invent synonyms or assessments", {
  fixture <- make_single_product_sources(c("Ácido cítrico", "Citric acid"))
  service <- application_env$new_excipient_suggestion_service(
    fixture$sources$product_source,
    fixture$sources$composition_source
  )
  suggestions <- service$suggest_excipients_for_active_ingredient("ingredient")
  values <- vapply(suggestions, `[[`, character(1), "value")

  expect_identical(values, c("Ácido cítrico", "Citric acid"))
  expect_false(any(vapply(suggestions, inherits, logical(1), "excipient_assessment")))
  expect_identical(search_call_count(fixture$sources, "get_source_content"), 0L)
})
