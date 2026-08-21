.search_assert_optional_string <- function(value, name) {
  if (!is.null(value)) {
    .excipient_assert_non_empty_string(value, name)
  }
  invisible(value)
}

search_input_max_chars <- function() {
  200L
}

validate_search_input_text <- function(value, label) {
  .excipient_assert_non_empty_string(label, "label")
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .excipient_application_abort(sprintf("%s no puede estar vacío.", label))
  }
  if (stringi::stri_detect_regex(value, "\\p{Cc}")) {
    .excipient_application_abort(
      sprintf("%s contiene caracteres de control no permitidos.", label)
    )
  }
  if (stringi::stri_length(value) > search_input_max_chars()) {
    .excipient_application_abort(sprintf(
      "%s no puede superar los %d caracteres.",
      label,
      search_input_max_chars()
    ))
  }
  invisible(value)
}

new_product_excipient_result <- function(
    product,
    formulations,
    presentations,
    assessment,
    source_artifacts = list()) {
  if (!inherits(product, "medicinal_product")) {
    .excipient_application_abort("`product` must be a `medicinal_product` object.")
  }
  if (!is.list(formulations) || !all(vapply(
      formulations,
      inherits,
      logical(1),
      "formulation"))) {
    .excipient_application_abort("`formulations` must contain only `formulation` objects.")
  }
  if (!is.list(presentations) || !all(vapply(
      presentations,
      inherits,
      logical(1),
      "presentation"))) {
    .excipient_application_abort("`presentations` must contain only `presentation` objects.")
  }
  if (!inherits(assessment, "excipient_assessment")) {
    .excipient_application_abort("`assessment` must be an `excipient_assessment` object.")
  }
  if (!identical(assessment$subject_id, medicinal_product_id(product))) {
    .excipient_application_abort(
      "`assessment$subject_id` must match `medicinal_product_id(product)`."
    )
  }
  if (!is.list(source_artifacts) || !all(vapply(
      source_artifacts,
      inherits,
      logical(1),
      "source_artifact"))) {
    .excipient_application_abort(
      "`source_artifacts` must contain only `source_artifact` objects."
    )
  }
  artifact_ids <- vapply(source_artifacts, `[[`, character(1), "id")
  if (anyDuplicated(artifact_ids)) {
    .excipient_application_abort("`source_artifacts` must have unique IDs.")
  }
  attempt_artifact_ids <- unlist(lapply(assessment$attempts, function(attempt) {
    attempt$source_artifact_id
  }), use.names = FALSE)
  if (length(attempt_artifact_ids) > 0L &&
      !all(attempt_artifact_ids %in% artifact_ids)) {
    .excipient_application_abort(
      "Every assessment attempt artifact must exist in `source_artifacts`."
    )
  }

  structure(
    list(
      product = product,
      formulations = unname(formulations),
      presentations = unname(presentations),
      assessment = assessment,
      source_artifacts = unname(source_artifacts)
    ),
    class = c("product_excipient_result", "excifinder_application_dto")
  )
}

new_excipient_search_error <- function(
    stage,
    subject_id = NULL,
    message,
    condition = NULL,
    code = NULL) {
  .excipient_assert_non_empty_string(stage, "stage")
  .search_assert_optional_string(subject_id, "subject_id")
  .excipient_assert_non_empty_string(message, "message")
  .search_assert_optional_string(code, "code")
  if (!is.null(condition) && !inherits(condition, "condition")) {
    .excipient_application_abort("`condition` must be NULL or a condition.")
  }

  structure(
    list(
      stage = stage,
      subject_id = subject_id,
      message = message,
      condition = condition,
      code = code
    ),
    class = c("excipient_search_error", "excifinder_application_dto")
  )
}

new_excipient_search_result <- function(
    query,
    resolution,
    results = list(),
    errors = list()) {
  if (!inherits(resolution, "excipient_resolution")) {
    .excipient_application_abort("`resolution` must be an `excipient_resolution` object.")
  }
  if (!is.list(results) || !all(vapply(
      results,
      inherits,
      logical(1),
      "product_excipient_result"))) {
    .excipient_application_abort(
      "`results` must contain only `product_excipient_result` objects."
    )
  }
  if (!is.list(errors) || !all(vapply(
      errors,
      inherits,
      logical(1),
      "excipient_search_error"))) {
    .excipient_application_abort(
      "`errors` must contain only `excipient_search_error` objects."
    )
  }

  structure(
    list(
      query = query,
      resolution = resolution,
      results = unname(results),
      errors = unname(errors)
    ),
    class = c("excipient_search_result", "excifinder_application_dto")
  )
}

.search_safe_call <- function(operation) {
  tryCatch(
    list(ok = TRUE, value = operation()),
    error = function(error) list(ok = FALSE, error = error)
  )
}

.search_error_from_condition <- function(stage, subject_id, condition) {
  new_excipient_search_error(
    stage = stage,
    subject_id = subject_id,
    message = conditionMessage(condition),
    condition = condition,
    code = "source_failure"
  )
}

.search_contract_error <- function(stage, subject_id, message) {
  new_excipient_search_error(
    stage = stage,
    subject_id = subject_id,
    message = message,
    code = "invalid_source_result"
  )
}

.search_unavailable_marker <- function(stage, subject_id, message) {
  structure(
    list(
      stage = stage,
      subject_id = subject_id,
      message = message,
      code = "source_absent"
    ),
    class = "excipient_source_unavailable"
  )
}

.search_validate_filters <- function(filters) {
  if (!is.list(filters) ||
      (length(filters) > 0L &&
        (is.null(names(filters)) || any(!nzchar(names(filters)))))) {
    .excipient_application_abort("`filters` must be a named list.")
  }
  unknown <- setdiff(names(filters), c("authorized", "marketed"))
  if (length(unknown) > 0L) {
    .excipient_application_abort(
      sprintf("Unsupported filters: %s.", paste(unknown, collapse = ", "))
    )
  }
  for (name in names(filters)) {
    value <- filters[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .excipient_application_abort(
        sprintf("`filters$%s` must be a non-missing logical scalar.", name)
      )
    }
  }
  filters
}

.search_validate_progress <- function(progress) {
  if (!is.null(progress) && !is.function(progress)) {
    .excipient_application_abort("`progress` must be NULL or a function.")
  }
  progress
}

.search_emit_progress <- function(
    progress,
    event,
    current,
    total,
    product_id = NULL,
    product_name = NULL) {
  if (is.null(progress)) {
    return(invisible(NULL))
  }
  tryCatch(
    progress(
      event = event,
      current = current,
      total = total,
      product_id = product_id,
      product_name = product_name
    ),
    error = function(error) NULL
  )
  invisible(NULL)
}

.search_order_products <- function(products) {
  if (length(products) < 2L) {
    return(unname(products))
  }
  names <- vapply(products, `[[`, character(1), "name")
  ids <- vapply(products, medicinal_product_id, character(1))
  unname(products[order(tolower(names), ids, method = "radix")])
}

.search_order_formulations <- function(formulations) {
  if (length(formulations) < 2L) {
    return(unname(formulations))
  }
  ids <- vapply(formulations, `[[`, character(1), "id")
  unname(formulations[order(ids, method = "radix")])
}

.search_order_presentations <- function(presentations) {
  if (length(presentations) == 0L) {
    return(list())
  }
  ids <- vapply(presentations, presentation_id, character(1))
  presentations <- presentations[!duplicated(ids)]
  ids <- vapply(presentations, presentation_id, character(1))
  descriptions <- vapply(presentations, `[[`, character(1), "description")
  unname(presentations[order(tolower(descriptions), ids, method = "radix")])
}

.search_source_date_number <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NA_real_)
  }
  if (inherits(value, "POSIXt")) {
    return(as.numeric(value))
  }
  if (inherits(value, "Date")) {
    return(as.numeric(as.POSIXct(value, tz = "UTC")))
  }
  if (is.character(value) && nzchar(trimws(value))) {
    parsed <- tryCatch(
      suppressWarnings(as.POSIXct(value, tz = "UTC")),
      error = function(error) as.POSIXct(NA)
    )
    if (!is.na(parsed)) {
      return(as.numeric(parsed))
    }
  }
  NA_real_
}

.search_select_document <- function(artifacts, subject_id) {
  if (length(artifacts) == 0L) {
    return(list(artifact = NULL, error = NULL))
  }
  if (length(artifacts) == 1L) {
    return(list(artifact = artifacts[[1]], error = NULL))
  }

  dates <- vapply(artifacts, function(artifact) {
    .search_source_date_number(artifact$source_date)
  }, numeric(1))
  usable <- which(!is.na(dates))
  selected <- integer()
  if (length(usable) == 1L) {
    selected <- usable
  } else if (length(usable) > 1L) {
    selected <- usable[dates[usable] == max(dates[usable])]
  }
  if (length(selected) == 1L) {
    return(list(artifact = artifacts[[selected]], error = NULL))
  }

  list(
    artifact = NULL,
    error = new_excipient_search_error(
      stage = "select_source_artifact",
      subject_id = subject_id,
      message = "Multiple equally eligible product documents were found.",
      code = "ambiguous_source_artifact"
    )
  )
}

.search_query_dto <- function(active_ingredient, excipient_query, filters) {
  list(
    active_ingredient = active_ingredient,
    excipient = excipient_query,
    filters = filters
  )
}

new_excipient_search_service <- function(
    product_source,
    composition_source,
    artifact_source,
    taxonomy,
    matcher_version,
    allow_literal_fallback = TRUE) {
  if (!is_product_source_port(product_source)) {
    .excipient_application_abort("`product_source` must be a `product_source_port`.")
  }
  if (!is_composition_source_port(composition_source)) {
    .excipient_application_abort(
      "`composition_source` must be a `composition_source_port`."
    )
  }
  if (!is_source_artifact_port(artifact_source)) {
    .excipient_application_abort("`artifact_source` must be a `source_artifact_port`.")
  }
  if (!is_excipient_taxonomy(taxonomy)) {
    .excipient_application_abort("`taxonomy` must be an `excipient_taxonomy` object.")
  }
  .excipient_assert_non_empty_string(matcher_version, "matcher_version")
  if (!is.logical(allow_literal_fallback) ||
      length(allow_literal_fallback) != 1L || is.na(allow_literal_fallback)) {
    .excipient_application_abort(
      "`allow_literal_fallback` must be a non-missing logical scalar."
    )
  }

  search <- function(
      active_ingredient,
      excipient_query,
      filters = list(authorized = TRUE, marketed = TRUE),
      progress = NULL) {
    progress <- .search_validate_progress(progress)
    validate_search_input_text(active_ingredient, "El principio activo")
    if (!is.character(excipient_query) || length(excipient_query) != 1L ||
        is.na(excipient_query)) {
      .excipient_application_abort(
        "`excipient_query` must be a single, non-missing character string."
      )
    }
    resolution_call <- .search_safe_call(function() {
      resolve_excipient_query(taxonomy, excipient_query)
    })
    if (resolution_call$ok) {
      resolution <- resolution_call$value
    } else {
      resolution <- new_excipient_resolution(
        query = excipient_query,
        status = "not_found",
        strategy = "taxonomy"
      )
    }
    if (identical(resolution$status, "ambiguous")) {
      return(new_excipient_search_result(
        query = .search_query_dto(active_ingredient, excipient_query, filters),
        resolution = resolution
      ))
    }
    assessment_taxonomy_version <- taxonomy$version
    if (!resolution_call$ok || identical(resolution$status, "not_found")) {
      if (!allow_literal_fallback) {
        errors <- if (resolution_call$ok) {
          list()
        } else {
          list(new_excipient_search_error(
            stage = "resolve_excipient",
            message = conditionMessage(resolution_call$error),
            condition = resolution_call$error,
            code = "invalid_excipient_query"
          ))
        }
        return(new_excipient_search_result(
          query = .search_query_dto(active_ingredient, excipient_query, filters),
          resolution = resolution,
          errors = errors
        ))
      }
      literal_call <- .search_safe_call(function() {
        new_literal_query_excipient(excipient_query)
      })
      if (!literal_call$ok) {
        return(new_excipient_search_result(
          query = .search_query_dto(active_ingredient, excipient_query, filters),
          resolution = resolution,
          errors = list(new_excipient_search_error(
            stage = "resolve_excipient",
            message = conditionMessage(literal_call$error),
            condition = literal_call$error,
            code = "invalid_literal_query"
          ))
        ))
      }
      excipient <- literal_call$value
      resolution <- new_literal_excipient_resolution(excipient_query, excipient)
      assessment_taxonomy_version <- literal_excipient_taxonomy_version()
    } else {
      excipient <- resolution$candidates[[1]]
    }
    filters <- .search_validate_filters(filters)
    query <- .search_query_dto(active_ingredient, excipient_query, filters)

    discovery <- .search_safe_call(function() {
      product_source$find_products_by_active_ingredient(active_ingredient, filters)
    })
    if (!discovery$ok) {
      return(new_excipient_search_result(
        query = query,
        resolution = resolution,
        errors = list(.search_error_from_condition(
          "find_products",
          NULL,
          discovery$error
        ))
      ))
    }
    products <- discovery$value
    if (!is.list(products) || !all(vapply(
        products,
        inherits,
        logical(1),
        "medicinal_product"))) {
      return(new_excipient_search_result(
        query = query,
        resolution = resolution,
        errors = list(.search_contract_error(
          "find_products",
          NULL,
          "Product discovery returned an invalid collection."
        ))
      ))
    }
    product_ids <- vapply(products, medicinal_product_id, character(1))
    products <- products[!duplicated(product_ids)]
    products <- .search_order_products(products)
    .search_emit_progress(
      progress,
      event = "products_discovered",
      current = 0L,
      total = length(products)
    )

    results <- list()
    errors <- list()
    append_error <- function(error) {
      errors[[length(errors) + 1L]] <<- error
    }

    for (product_index in seq_along(products)) {
      summary <- products[[product_index]]
      .search_emit_progress(
        progress,
        event = "product_started",
        current = product_index - 1L,
        total = length(products),
        product_id = medicinal_product_id(summary),
        product_name = summary$name
      )
      summary_id <- medicinal_product_id(summary)
      detail_call <- .search_safe_call(function() product_source$get_product(summary_id))
      if (!detail_call$ok) {
        append_error(.search_error_from_condition(
          "get_product",
          summary_id,
          detail_call$error
        ))
        next
      }
      if (is_port_absent(detail_call$value)) {
        append_error(new_excipient_search_error(
          "get_product",
          summary_id,
          "Product detail is unavailable.",
          code = "source_absent"
        ))
        next
      }
      product <- detail_call$value
      if (!inherits(product, "medicinal_product") ||
          !identical(medicinal_product_id(product), summary_id)) {
        append_error(.search_contract_error(
          "get_product",
          summary_id,
          "Product detail returned an invalid or mismatched product."
        ))
        next
      }
      product_id <- medicinal_product_id(product)

      formulation_call <- .search_safe_call(function() {
        product_source$get_formulations(product_id)
      })
      structured_errors <- list()
      if (!formulation_call$ok) {
        formulation_error <- .search_error_from_condition(
          "get_formulations",
          product_id,
          formulation_call$error
        )
        append_error(formulation_error)
        structured_errors <- list(formulation_error)
        formulations <- list()
      } else if (is_port_absent(formulation_call$value)) {
        formulations <- list()
      } else if (!is.list(formulation_call$value) || !all(vapply(
          formulation_call$value,
          inherits,
          logical(1),
          "formulation"))) {
        formulation_error <- .search_contract_error(
          "get_formulations",
          product_id,
          "Formulation retrieval returned an invalid collection."
        )
        append_error(formulation_error)
        structured_errors <- list(formulation_error)
        formulations <- list()
      } else {
        formulations <- .search_order_formulations(formulation_call$value)
      }

      structured_snapshots <- list()
      source_artifacts <- list()
      presentations <- list()
      for (formulation in formulations) {
        formulation_id <- formulation$id
        composition_call <- .search_safe_call(function() {
          composition_source$get_composition_snapshot(formulation_id)
        })
        if (!composition_call$ok) {
          composition_error <- .search_error_from_condition(
            "get_composition_snapshot",
            formulation_id,
            composition_call$error
          )
          append_error(composition_error)
          structured_errors[[length(structured_errors) + 1L]] <- composition_error
        } else if (is_port_absent(composition_call$value)) {
          structured_errors[[length(structured_errors) + 1L]] <-
            .search_unavailable_marker(
              "get_composition_snapshot",
              formulation_id,
              "Structured composition is unavailable."
            )
        } else {
          structured_snapshots[[length(structured_snapshots) + 1L]] <-
            composition_call$value
          source_artifacts[[length(source_artifacts) + 1L]] <-
            composition_call$value$source_artifact
        }

        presentation_call <- .search_safe_call(function() {
          product_source$get_presentations(formulation_id, filters)
        })
        if (!presentation_call$ok) {
          append_error(.search_error_from_condition(
            "get_presentations",
            formulation_id,
            presentation_call$error
          ))
        } else if (!is_port_absent(presentation_call$value)) {
          if (!is.list(presentation_call$value) || !all(vapply(
              presentation_call$value,
              inherits,
              logical(1),
              "presentation"))) {
            append_error(.search_contract_error(
              "get_presentations",
              formulation_id,
              "Presentation retrieval returned an invalid collection."
            ))
          } else {
            presentations <- c(presentations, presentation_call$value)
          }
        }
      }
      presentations <- .search_order_presentations(presentations)

      smpc_artifact <- NULL
      smpc_content <- NULL
      smpc_error <- NULL
      artifact_call <- .search_safe_call(function() {
        artifact_source$list_source_artifacts(
          product_id,
          artifact_type = "document"
        )
      })
      if (!artifact_call$ok) {
        smpc_error <- .search_error_from_condition(
          "list_source_artifacts",
          product_id,
          artifact_call$error
        )
        append_error(smpc_error)
      } else {
        artifacts <- artifact_call$value
        if (is_port_absent(artifacts)) {
          artifacts <- list()
        }
        if (!is.list(artifacts) || !all(vapply(
            artifacts,
            inherits,
            logical(1),
            "source_artifact"))) {
          smpc_error <- .search_contract_error(
            "list_source_artifacts",
            product_id,
            "Document discovery returned an invalid collection."
          )
          append_error(smpc_error)
        } else {
          artifacts <- Filter(function(artifact) {
            identical(
              artifact$artifact_kind,
              "summary_of_product_characteristics"
            )
          }, artifacts)
          source_artifacts <- c(source_artifacts, artifacts)
          selection <- .search_select_document(artifacts, product_id)
          if (!is.null(selection$error)) {
            smpc_error <- selection$error
            append_error(selection$error)
          } else if (is.null(selection$artifact)) {
            smpc_error <- .search_unavailable_marker(
              "list_source_artifacts",
              product_id,
              "No eligible product document is available."
            )
          } else {
            selected_artifact <- selection$artifact
            content_call <- .search_safe_call(function() {
              artifact_source$get_source_content(
                selected_artifact$id,
                section = "6.1"
              )
            })
            if (!content_call$ok) {
              smpc_error <- .search_error_from_condition(
                "get_source_content",
                selected_artifact$id,
                content_call$error
              )
              append_error(smpc_error)
            } else if (is_port_absent(content_call$value)) {
              smpc_error <- .search_unavailable_marker(
                "get_source_content",
                selected_artifact$id,
                "Requested document content is unavailable."
              )
            } else if (!is_source_content(content_call$value)) {
              smpc_error <- .search_contract_error(
                "get_source_content",
                selected_artifact$id,
                "Document content retrieval returned an invalid value."
              )
              append_error(smpc_error)
            } else {
              smpc_artifact <- selected_artifact
              smpc_content <- content_call$value
            }
          }
        }
      }

      assessment <- assess_excipient_from_retrieved_sources(
        subject_id = product_id,
        excipient = excipient,
        taxonomy_version = assessment_taxonomy_version,
        matcher_version = matcher_version,
        structured_snapshots = structured_snapshots,
        smpc_artifact = smpc_artifact,
        smpc_content = smpc_content,
        structured_errors = structured_errors,
        smpc_error = smpc_error
      )
      if (length(source_artifacts) > 0L) {
        artifact_ids <- vapply(source_artifacts, `[[`, character(1), "id")
        source_artifacts <- source_artifacts[!duplicated(artifact_ids)]
      }
      results[[length(results) + 1L]] <- new_product_excipient_result(
        product,
        formulations,
        presentations,
        assessment,
        source_artifacts = source_artifacts
      )
    }

    if (length(results) > 1L) {
      result_names <- vapply(results, function(result) {
        result$product$name
      }, character(1))
      result_ids <- vapply(results, function(result) {
        medicinal_product_id(result$product)
      }, character(1))
      results <- results[order(tolower(result_names), result_ids, method = "radix")]
    }
    .search_emit_progress(
      progress,
      event = "complete",
      current = length(products),
      total = length(products)
    )
    new_excipient_search_result(query, resolution, results, errors)
  }

  structure(
    list(search_excipient = search),
    class = c("excipient_search_service", "excifinder_application_service")
  )
}
