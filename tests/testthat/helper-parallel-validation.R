sys.source(
  file.path(project_root(), "tests", "validation", "validation_cases.R"),
  envir = environment()
)

.parallel_validation_document_text <- function(scenario) {
  if (!is.null(scenario$fixture)) {
    return(paste(
      readLines(
        file.path(
          project_root(),
          "tests",
          "fixtures",
          "cima-v1.23",
          scenario$fixture
        ),
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    ))
  }
  if (is.null(scenario$document_text)) "" else scenario$document_text
}

.parallel_validation_legacy_mock <- function(scenario) {
  zero_medicines <- isTRUE(scenario$zero_medicines)
  document_status <- if (is.null(scenario$document_status)) {
    200L
  } else {
    as.integer(scenario$document_status)
  }
  medicine_body <- jsonlite::toJSON(
    list(resultados = if (zero_medicines) list() else list(list(
      nregistro = "parallel-001",
      nombre = "Parallel validation product",
      docs = list(list(tipo = 1L, url = "#"))
    ))),
    auto_unbox = TRUE
  )
  document_body <- jsonlite::toJSON(
    list(list(
      seccion = "6.1",
      contenido = .parallel_validation_document_text(scenario)
    )),
    auto_unbox = TRUE
  )

  function(url, query = NULL, ...) {
    if (identical(url, "https://cima.aemps.es/cima/rest/medicamentos")) {
      return(mock_response(medicine_body, 200L))
    }
    if (startsWith(
        url,
        "https://cima.aemps.es/cima/rest/docSegmentado/contenido/1?nregistro=")) {
      body <- if (identical(document_status, 200L)) document_body else "{}"
      return(mock_response(body, document_status))
    }
    stop(sprintf("Unexpected external request in validation: %s", url), call. = FALSE)
  }
}

run_parallel_legacy_case <- function(case) {
  observed <- tryCatch(
    run_search(
      .parallel_validation_legacy_mock(case$legacy_scenario),
      pa = "parallel ingredient",
      excipiente = case$excipient_query
    ),
    error = function(error) error
  )
  state <- if (inherits(observed, "error")) {
    "legacy_error"
  } else if (is.null(observed)) {
    "legacy_no_result"
  } else if (nrow(observed) == 0L) {
    "legacy_no_products"
  } else if (isTRUE(observed$estado[[1]])) {
    "legacy_positive"
  } else {
    "legacy_negative"
  }
  list(state = state, raw = observed)
}

.parallel_validation_taxonomy <- function(scenario, query) {
  if (identical(scenario$taxonomy_mode, "ambiguous")) {
    return(make_search_taxonomy(list(
      domain_env$new_excipient(
        "parallel-concept-a",
        "parallel concept a",
        synonyms = query
      ),
      domain_env$new_excipient(
        "parallel-concept-b",
        "parallel concept b",
        language_variants = query
      )
    )))
  }
  make_search_taxonomy()
}

.parallel_validation_new_sources <- function(scenario) {
  product <- make_search_product("parallel-001", "Parallel validation product")
  product_id <- domain_env$medicinal_product_id(product)
  if (isTRUE(scenario$zero_products)) {
    return(list(
      sources = new_search_fake_sources(products = list()),
      product = product
    ))
  }

  entry_sets <- if (is.null(scenario$structured_entries)) {
    list()
  } else {
    scenario$structured_entries
  }
  formulations <- lapply(seq_along(entry_sets), function(index) {
    make_search_formulation(product, as.character(index))
  })
  snapshots <- lapply(seq_along(formulations), function(index) {
    make_search_snapshot(
      formulations[[index]],
      entry_sets[[index]],
      as.character(index)
    )
  })
  snapshot_map <- if (length(snapshots) == 0L) {
    list()
  } else {
    setNames(snapshots, vapply(formulations, `[[`, character(1), "id"))
  }

  mode <- if (is.null(scenario$document_mode)) "valid" else scenario$document_mode
  document_text <- .parallel_validation_document_text(scenario)
  artifacts <- list()
  contents <- list()
  if (identical(mode, "ambiguous")) {
    same_date <- as.POSIXct("2026-01-01", tz = "UTC")
    artifacts <- list(
      make_search_document(product, "ambiguous-a", same_date),
      make_search_document(product, "ambiguous-b", same_date)
    )
  } else if (!identical(mode, "absent")) {
    artifact <- make_search_document(product, "validation")
    artifacts <- list(artifact)
    content <- if (identical(mode, "failed")) {
      make_search_source_failure("controlled document retrieval failure")
    } else if (identical(mode, "unsupported")) {
      application_env$new_source_content(
        source_artifact_id = artifact$id,
        content = document_text,
        content_type = "text/html",
        section = "6.1",
        retrieval_method = "controlled_unsupported_content"
      )
    } else {
      make_search_content(artifact, document_text)
    }
    contents <- setNames(list(content), artifact$id)
  }

  list(
    product = product,
    sources = new_search_fake_sources(
      products = list(product),
      details = setNames(list(product), product_id),
      formulations = setNames(list(formulations), product_id),
      snapshots = snapshot_map,
      artifacts = setNames(list(artifacts), product_id),
      contents = contents
    )
  )
}

run_parallel_new_scenario <- function(scenario, excipient_query) {
  fixture <- .parallel_validation_new_sources(scenario)
  service <- make_search_service(
    fixture$sources,
    taxonomy = .parallel_validation_taxonomy(scenario, excipient_query)
  )
  result <- service$search_excipient(
    "parallel ingredient",
    excipient_query
  )
  state <- if (identical(result$resolution$status, "ambiguous")) {
    "new_query_ambiguous"
  } else if (any(vapply(result$errors, function(error) {
      identical(error$code, "invalid_literal_query")
    }, logical(1)))) {
    "new_invalid_query"
  } else if (length(result$results) == 0L) {
    "new_no_products"
  } else {
    switch(
      result$results[[1]]$assessment$factual_conclusion,
      identified = "new_identified",
      not_identified = "new_not_identified",
      indeterminate = "new_indeterminate",
      conflicting = "new_conflicting"
    )
  }
  list(
    state = state,
    coverage = if (length(result$results) == 0L) {
      NULL
    } else {
      result$results[[1]]$assessment$verification_coverage
    },
    strategy = result$resolution$strategy,
    result_count = length(result$results),
    source_calls = search_call_count(fixture$sources),
    raw = result
  )
}

run_parallel_new_case <- function(case) {
  run_parallel_new_scenario(case$new_scenario, case$excipient_query)
}

.parallel_validation_cache <- new.env(parent = emptyenv())

parallel_validation_observations <- function() {
  if (!exists("observations", envir = .parallel_validation_cache, inherits = FALSE)) {
    cases <- parallel_validation_cases()
    observations <- lapply(cases, function(case) {
      list(
        case = case,
        legacy = run_parallel_legacy_case(case),
        new = run_parallel_new_case(case)
      )
    })
    assign("observations", observations, envir = .parallel_validation_cache)
  }
  get("observations", envir = .parallel_validation_cache, inherits = FALSE)
}

.parallel_validation_case_passed <- function(observation) {
  case <- observation$case
  identical(observation$legacy$state, case$legacy_expected) &&
    identical(observation$new$state, case$gold_expected) &&
    identical(observation$new$result_count, case$new_expected_result_count) &&
    (is.null(case$new_expected_coverage) ||
      identical(observation$new$coverage, case$new_expected_coverage)) &&
    (is.null(case$new_expected_strategy) ||
      identical(observation$new$strategy, case$new_expected_strategy)) &&
    (is.null(case$new_expected_source_calls) ||
      identical(observation$new$source_calls, case$new_expected_source_calls))
}

parallel_validation_matrix <- function() {
  observations <- parallel_validation_observations()
  rows <- lapply(observations, function(observation) {
    data.frame(
      case_id = observation$case$case_id,
      legacy_observed = observation$legacy$state,
      new_observed = observation$new$state,
      comparison_expectation = observation$case$comparison_expectation,
      comparison_passed = .parallel_validation_case_passed(observation),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
