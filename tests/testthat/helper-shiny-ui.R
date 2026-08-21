make_ui_search_result <- function(
    conclusion = "identified",
    coverage = "complete",
    strategy = "literal",
    registration_number = "ui-001",
    product_name = "UI Product",
    pharmaceutical_form = "Comprimido",
    strength = "500 mg",
    routes = "Oral",
    active_ingredient_query = "ingredient",
    excipient_query = "lactosa",
    evidence_excerpts = "Original lactose excerpt",
    smpc_urls = "https://example.test/document.pdf",
    include_structured = FALSE,
    errors = list()) {
  product <- make_search_product(registration_number, product_name)
  product_id <- domain_env$medicinal_product_id(product)
  formulation <- domain_env$new_formulation(
    id = paste0(product_id, ":formulation:1"),
    medicinal_product_id = product_id,
    pharmaceutical_form = pharmaceutical_form,
    routes = routes,
    strength = strength
  )
  excipient <- domain_env$new_excipient("ui-excipient", excipient_query)
  artifacts <- list()
  attempts <- list()

  if (include_structured) {
    structured <- domain_env$new_source_artifact(
      id = "ui-structured",
      source = "official_structured_source",
      subject_id = paste0(product_id, ":formulation:1"),
      artifact_type = "structured_record",
      artifact_kind = "medicinal_product_record"
    )
    artifacts[[length(artifacts) + 1L]] <- structured
    attempts[[length(attempts) + 1L]] <- domain_env$new_verification_attempt(
      source = structured$source,
      source_artifact_id = structured$id,
      method = "structured_fields",
      outcome = "no_evidence",
      extraction_status = "complete"
    )
  }

  document_count <- max(length(smpc_urls), if (length(evidence_excerpts) > 0L) 1L else 0L)
  documents <- lapply(seq_len(document_count), function(index) {
    domain_env$new_source_artifact(
      id = paste0("ui-document-", index),
      source = "official_document_source",
      subject_id = product_id,
      artifact_type = "document",
      artifact_kind = "summary_of_product_characteristics",
      url = if (index <= length(smpc_urls)) smpc_urls[[index]] else NULL
    )
  })
  artifacts <- c(artifacts, documents)

  if (length(evidence_excerpts) > 0L) {
    evidence <- lapply(seq_along(evidence_excerpts), function(index) {
      domain_env$new_excipient_evidence(
        id = paste0("ui-evidence-", index),
        excipient_id = excipient$id,
        subject_id = product_id,
        source_artifact_id = documents[[1]]$id,
        matched_term = "lactosa",
        section = "6.1",
        excerpt = evidence_excerpts[[index]],
        method = "controlled_terms",
        location = list(start = index, end = index + 1L)
      )
    })
    attempts[[length(attempts) + 1L]] <- domain_env$new_verification_attempt(
      source = documents[[1]]$source,
      source_artifact_id = documents[[1]]$id,
      method = "controlled_terms",
      section = "6.1",
      outcome = "evidence_found",
      extraction_status = "complete",
      evidence = evidence
    )
  } else if (length(documents) > 0L) {
    attempts[[length(attempts) + 1L]] <- domain_env$new_verification_attempt(
      source = documents[[1]]$source,
      source_artifact_id = documents[[1]]$id,
      method = "controlled_terms",
      section = "6.1",
      outcome = if (identical(conclusion, "not_identified")) "no_evidence" else "inconclusive",
      extraction_status = if (identical(coverage, "complete")) "complete" else "failed"
    )
  }

  assessment <- domain_env$new_excipient_assessment(
    subject_id = product_id,
    excipient_id = excipient$id,
    factual_conclusion = conclusion,
    verification_coverage = coverage,
    attempts = attempts,
    matcher_version = "ui-matcher",
    taxonomy_version = if (identical(strategy, "literal")) "literal-v1" else "ui-taxonomy"
  )
  product_result <- application_env$new_product_excipient_result(
    product,
    list(formulation),
    list(),
    assessment,
    source_artifacts = artifacts
  )
  resolution <- application_env$new_excipient_resolution(
    query = excipient_query,
    status = "resolved",
    strategy = strategy,
    candidates = list(excipient)
  )
  application_env$new_excipient_search_result(
    query = list(
      active_ingredient = active_ingredient_query,
      excipient = excipient_query
    ),
    resolution = resolution,
    results = list(product_result),
    errors = errors
  )
}

make_ui_ambiguous_result <- function() {
  candidates <- list(
    domain_env$new_excipient("ui-a", "Concepto A"),
    domain_env$new_excipient("ui-b", "Concepto B")
  )
  resolution <- application_env$new_excipient_resolution(
    "alias",
    "ambiguous",
    candidates = candidates
  )
  application_env$new_excipient_search_result("alias", resolution)
}

make_ui_empty_result <- function() {
  excipient <- domain_env$new_excipient("ui-empty", "lactosa")
  resolution <- application_env$new_excipient_resolution(
    "lactosa",
    "resolved",
    candidates = list(excipient),
    strategy = "literal"
  )
  application_env$new_excipient_search_result("lactosa", resolution)
}

make_ui_invalid_result <- function() {
  resolution <- application_env$new_excipient_resolution(
    " ",
    "not_found",
    strategy = "taxonomy"
  )
  error <- application_env$new_excipient_search_error(
    stage = "resolve_excipient",
    message = "Controlled invalid query",
    code = "invalid_literal_query"
  )
  application_env$new_excipient_search_result(
    " ",
    resolution,
    errors = list(error)
  )
}

make_ui_partial_error <- function() {
  application_env$new_excipient_search_error(
    stage = "get_source_content",
    subject_id = "AUTH:ui-001",
    message = "Controlled source failure",
    code = "source_failure"
  )
}

new_ui_fake_search_service <- function(result, progress_events = list()) {
  calls <- new.env(parent = emptyenv())
  calls$items <- list()
  calls$progress <- list()
  service <- list(search_excipient = function(
      active_ingredient,
      excipient_query,
      filters,
      progress = NULL) {
    calls$items[[length(calls$items) + 1L]] <- list(
      active_ingredient = active_ingredient,
      excipient_query = excipient_query,
      filters = filters
    )
    calls$progress[[length(calls$progress) + 1L]] <- progress
    for (event in progress_events) {
      do.call(progress, event)
    }
    result
  })
  list(service = service, calls = calls)
}

new_ui_sequential_fake_search_service <- function(results) {
  calls <- new.env(parent = emptyenv())
  calls$items <- list()
  service <- list(search_excipient = function(
      active_ingredient,
      excipient_query,
      filters,
      progress = NULL) {
    calls$items[[length(calls$items) + 1L]] <- list(
      active_ingredient = active_ingredient,
      excipient_query = excipient_query,
      filters = filters
    )
    results[[min(length(calls$items), length(results))]]
  })
  list(service = service, calls = calls)
}

make_ui_mixed_result <- function(conclusions = c(
    "identified", "not_identified", "indeterminate", "conflicting")) {
  results <- lapply(seq_along(conclusions), function(index) {
    make_ui_search_result(
      conclusion = conclusions[[index]],
      registration_number = paste0("ui-00", index),
      product_name = paste("UI Product", index),
      evidence_excerpts = if (conclusions[[index]] %in%
          c("identified", "conflicting")) "Evidence" else character()
    )
  })
  mixed <- results[[1]]
  mixed$results <- lapply(results, function(result) result$results[[1]])
  mixed
}

new_ui_fake_suggestion_source <- function(values = character(), fail = FALSE) {
  calls <- new.env(parent = emptyenv())
  calls$items <- list()
  source <- application_env$new_suggestion_source_port(function(query, limit) {
    calls$items[[length(calls$items) + 1L]] <- list(query = query, limit = limit)
    if (fail) stop("controlled suggestion failure")
    lapply(seq_len(min(length(values), limit)), function(index) {
      application_env$new_suggestion(
        paste0("suggestion-", index), values[[index]], values[[index]]
      )
    })
  })
  list(source = source, calls = calls)
}

new_ui_fake_excipient_suggestion_service <- function(
    values = character(),
    fail = FALSE) {
  calls <- new.env(parent = emptyenv())
  calls$items <- list()
  service <- list(suggest_excipients_for_active_ingredient = function(
      active_ingredient,
      limit) {
    calls$items[[length(calls$items) + 1L]] <- list(
      active_ingredient = active_ingredient,
      limit = limit
    )
    if (fail) stop("controlled excipient suggestion failure")
    lapply(seq_len(min(length(values), limit)), function(index) {
      application_env$new_suggestion(
        paste0("excipient-", index), values[[index]], values[[index]]
      )
    })
  })
  list(service = service, calls = calls)
}
