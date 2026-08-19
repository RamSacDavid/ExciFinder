make_search_product <- function(registration_number, name) {
  domain_env$new_medicinal_product(
    authority = "AUTH",
    registration_number = registration_number,
    name = name,
    authorization_status = "authorized",
    is_marketed = TRUE
  )
}

make_search_formulation <- function(product, suffix = "1") {
  product_id <- domain_env$medicinal_product_id(product)
  domain_env$new_formulation(
    id = paste(product_id, "formulation", suffix, sep = ":"),
    medicinal_product_id = product_id,
    pharmaceutical_form = "Tablet"
  )
}

make_search_presentation <- function(formulation, code, description) {
  domain_env$new_presentation(
    authority = "AUTH",
    national_code = code,
    formulation_id = formulation$id,
    description = description,
    authorization_status = "authorized",
    is_marketed = TRUE
  )
}

make_search_snapshot <- function(formulation, entry_names, suffix = "1") {
  artifact <- domain_env$new_source_artifact(
    id = paste(formulation$id, "composition", suffix, sep = ":"),
    source = "official_structured_source",
    subject_id = formulation$id,
    artifact_type = "structured_record",
    artifact_kind = "medicinal_product_record"
  )
  entries <- lapply(seq_along(entry_names), function(index) {
    application_env$new_source_excipient_entry(
      source_artifact_id = artifact$id,
      source_record_id = paste0("entry-", index),
      subject_id = formulation$id,
      name = entry_names[[index]],
      position = index
    )
  })
  application_env$new_source_composition_snapshot(artifact, entries)
}

make_search_document <- function(product, suffix = "1", source_date = NULL) {
  domain_env$new_source_artifact(
    id = paste(domain_env$medicinal_product_id(product), "document", suffix, sep = ":"),
    source = "official_document_source",
    subject_id = domain_env$medicinal_product_id(product),
    artifact_type = "document",
    artifact_kind = "summary_of_product_characteristics",
    source_date = source_date
  )
}

make_search_content <- function(artifact, content) {
  application_env$new_source_content(
    source_artifact_id = artifact$id,
    content = content,
    content_type = "text/plain",
    section = "6.1",
    retrieval_method = "section_plain_text"
  )
}

make_search_taxonomy <- function(excipients = list(
    domain_env$new_excipient("excipient-lactose", "lactosa"))) {
  application_env$new_excipient_taxonomy("taxonomy-search-1", excipients)
}

make_search_source_failure <- function(message = "source failed") {
  simpleError(message)
}

new_search_fake_sources <- function(
    products = list(),
    details = list(),
    formulations = list(),
    snapshots = list(),
    presentations = list(),
    artifacts = list(),
    contents = list()) {
  calls <- new.env(parent = emptyenv())
  calls$events <- list()
  record <- function(operation, subject_id = NULL, filters = NULL) {
    calls$events[[length(calls$events) + 1L]] <- list(
      operation = operation,
      subject_id = subject_id,
      filters = filters
    )
  }
  materialize <- function(value) {
    if (inherits(value, "condition")) {
      stop(value)
    }
    value
  }
  lookup <- function(values, key, default) {
    if (!key %in% names(values)) {
      return(default)
    }
    materialize(values[[key]])
  }

  product_source <- application_env$new_product_source_port(
    find_products_by_active_ingredient = function(active_ingredient, filters) {
      record("find_products", active_ingredient, filters)
      materialize(products)
    },
    get_product = function(product_id) {
      record("get_product", product_id)
      lookup(details, product_id, application_env$new_port_absent())
    },
    get_formulations = function(product_id) {
      record("get_formulations", product_id)
      lookup(formulations, product_id, list())
    },
    get_presentations = function(formulation_id, filters) {
      record("get_presentations", formulation_id, filters)
      lookup(presentations, formulation_id, list())
    }
  )
  composition_source <- application_env$new_composition_source_port(
    get_composition_snapshot = function(subject_id) {
      record("get_composition_snapshot", subject_id)
      lookup(snapshots, subject_id, application_env$new_port_absent())
    }
  )
  artifact_source <- application_env$new_source_artifact_port(
    list_source_artifacts = function(subject_id, artifact_type = NULL) {
      record("list_source_artifacts", subject_id)
      lookup(artifacts, subject_id, list())
    },
    get_source_artifact = function(source_artifact_id) {
      record("get_source_artifact", source_artifact_id)
      application_env$new_port_absent()
    },
    get_source_content = function(source_artifact_id, section = NULL) {
      record("get_source_content", source_artifact_id)
      lookup(contents, source_artifact_id, application_env$new_port_absent())
    }
  )

  list(
    product_source = product_source,
    composition_source = composition_source,
    artifact_source = artifact_source,
    calls = calls
  )
}

make_search_service <- function(
    sources,
    taxonomy = make_search_taxonomy(),
    allow_literal_fallback = TRUE) {
  application_env$new_excipient_search_service(
    product_source = sources$product_source,
    composition_source = sources$composition_source,
    artifact_source = sources$artifact_source,
    taxonomy = taxonomy,
    matcher_version = "matcher-search-1",
    allow_literal_fallback = allow_literal_fallback
  )
}

search_call_count <- function(sources, operation = NULL) {
  events <- sources$calls$events
  if (is.null(operation)) {
    return(length(events))
  }
  sum(vapply(events, function(event) {
    identical(event$operation, operation)
  }, logical(1)))
}

make_single_product_sources <- function(
    structured_names,
    document_content = NULL,
    document_value = NULL) {
  product <- make_search_product("100", "Product One")
  product_id <- domain_env$medicinal_product_id(product)
  formulation <- make_search_formulation(product)
  snapshot <- make_search_snapshot(formulation, structured_names)
  document <- make_search_document(product)
  if (is.null(document_value)) {
    document_value <- if (is.null(document_content)) {
      application_env$new_port_absent()
    } else {
      make_search_content(document, document_content)
    }
  }
  sources <- new_search_fake_sources(
    products = list(product),
    details = setNames(list(product), product_id),
    formulations = setNames(list(list(formulation)), product_id),
    snapshots = setNames(list(snapshot), formulation$id),
    artifacts = setNames(list(list(document)), product_id),
    contents = setNames(list(document_value), document$id)
  )
  list(
    product = product,
    formulation = formulation,
    document = document,
    sources = sources,
    service = make_search_service(sources)
  )
}
