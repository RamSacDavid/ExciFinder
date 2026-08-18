.cima_sources_abort <- function(message, operation) {
  stop(new_port_error(message, "cima_sources", operation))
}

.cima_registration_from_product_id <- function(product_id) {
  if (!is.character(product_id) || length(product_id) != 1L || is.na(product_id) ||
      !startsWith(product_id, "AEMPS:")) {
    .cima_sources_abort("Product ID is outside the AEMPS namespace.", "product_id")
  }
  registration_number <- substring(product_id, nchar("AEMPS:") + 1L)
  if (!nzchar(registration_number) || grepl(":", registration_number, fixed = TRUE)) {
    .cima_sources_abort("Product ID has an invalid AEMPS registration part.", "product_id")
  }
  registration_number
}

.default_cima_product_id_from_formulation_id <- function(formulation_id) {
  suffix <- ":formulation:1"
  if (!is.character(formulation_id) || length(formulation_id) != 1L ||
      is.na(formulation_id) || !endsWith(formulation_id, suffix)) {
    .cima_sources_abort(
      "Formulation ID is outside the provisional CIMA namespace.",
      "formulation_id"
    )
  }
  product_id <- substr(formulation_id, 1L, nchar(formulation_id) - nchar(suffix))
  .cima_registration_from_product_id(product_id)
  product_id
}

new_cima_formulation_identity_strategy <- function(
    make = default_cima_formulation_id_factory,
    product_id_from_formulation_id = .default_cima_product_id_from_formulation_id) {
  if (!is.function(make) || !is.function(product_id_from_formulation_id)) {
    .cima_sources_abort(
      "Formulation identity strategy requires `make` and parser functions.",
      "identity_strategy"
    )
  }
  list(
    make = make,
    product_id_from_formulation_id = product_id_from_formulation_id
  )
}

.cima_filters <- function(filters) {
  if (is.null(filters)) {
    return(list())
  }
  if (!is.list(filters)) {
    .cima_sources_abort("`filters` must be a list.", "filters")
  }
  unknown <- setdiff(names(filters), c("authorized", "marketed"))
  if (length(unknown) > 0L) {
    .cima_sources_abort(
      sprintf("Unsupported canonical filters: %s.", paste(unknown, collapse = ", ")),
      "filters"
    )
  }
  filters
}

.cima_raw_detail_or_absent <- function(client, product_id) {
  registration_number <- .cima_registration_from_product_id(product_id)
  raw <- client$get_medicine(registration_number)
  if (length(raw) == 0L) new_port_absent() else raw
}

.cima_filter_presentations <- function(presentations, filters) {
  filters <- .cima_filters(filters)
  if (!is.null(filters$authorized)) {
    if (!is.logical(filters$authorized) || length(filters$authorized) != 1L ||
        is.na(filters$authorized)) {
      .cima_sources_abort("`authorized` must be logical.", "filters")
    }
    presentations <- Filter(function(presentation) {
      is_authorized <- identical(presentation$authorization_status, "authorized")
      identical(is_authorized, filters$authorized)
    }, presentations)
  }
  if (!is.null(filters$marketed)) {
    if (!is.logical(filters$marketed) || length(filters$marketed) != 1L ||
        is.na(filters$marketed)) {
      .cima_sources_abort("`marketed` must be logical.", "filters")
    }
    presentations <- Filter(function(presentation) {
      identical(presentation$is_marketed, filters$marketed)
    }, presentations)
  }
  unname(presentations)
}

new_cima_product_source_port <- function(
    client,
    formulation_identity = new_cima_formulation_identity_strategy()) {
  if (!is_cima_client(client)) {
    .cima_sources_abort("`client` must be a CIMA client.", "construction")
  }

  new_product_source_port(
    find_products_by_active_ingredient = function(active_ingredient, filters) {
      filters <- .cima_filters(filters)
      raw_medicines <- client$find_all_medicines(
        active_ingredient = active_ingredient,
        authorized = filters$authorized,
        marketed = filters$marketed
      )
      lapply(raw_medicines, map_cima_medicine_summary)
    },
    get_product = function(product_id) {
      raw <- .cima_raw_detail_or_absent(client, product_id)
      if (is_port_absent(raw)) raw else map_cima_medicinal_product(raw)
    },
    get_formulations = function(product_id) {
      raw <- .cima_raw_detail_or_absent(client, product_id)
      if (is_port_absent(raw)) {
        return(list())
      }
      product <- map_cima_medicinal_product(raw)
      list(map_cima_formulation(raw, product, formulation_identity$make))
    },
    get_presentations = function(formulation_id, filters) {
      product_id <- formulation_identity$product_id_from_formulation_id(
        formulation_id
      )
      raw <- .cima_raw_detail_or_absent(client, product_id)
      if (is_port_absent(raw)) {
        return(list())
      }
      product <- map_cima_medicinal_product(raw)
      formulation <- map_cima_formulation(
        raw,
        product,
        formulation_identity$make
      )
      if (!identical(formulation$id, formulation_id)) {
        .cima_sources_abort(
          "Formulation identity strategy is not reversible for this ID.",
          "get_presentations"
        )
      }
      presentations <- map_cima_presentations(raw, formulation$id)
      .cima_filter_presentations(presentations, filters)
    }
  )
}

new_cima_composition_source_port <- function(
    client,
    formulation_identity = new_cima_formulation_identity_strategy(),
    retrieved_at = function() Sys.time()) {
  if (!is_cima_client(client)) {
    .cima_sources_abort("`client` must be a CIMA client.", "construction")
  }
  if (!is.function(retrieved_at)) {
    .cima_sources_abort("`retrieved_at` must be a function.", "construction")
  }

  # Structured excipient entries are not assumed exhaustive for absence assessment.
  new_composition_source_port(
    list_excipient_entries = function(subject_id) {
      product_id <- formulation_identity$product_id_from_formulation_id(subject_id)
      raw <- .cima_raw_detail_or_absent(client, product_id)
      if (is_port_absent(raw)) {
        return(list())
      }
      product <- map_cima_medicinal_product(raw)
      formulation <- map_cima_formulation(
        raw,
        product,
        formulation_identity$make
      )
      if (!identical(formulation$id, subject_id)) {
        .cima_sources_abort(
          "Formulation identity strategy is not reversible for this ID.",
          "list_excipient_entries"
        )
      }
      artifact <- map_cima_structured_source_artifact(
        raw,
        formulation$id,
        retrieved_at = retrieved_at()
      )
      map_cima_source_excipient_entries(raw, artifact$id, formulation$id)
    }
  )
}
