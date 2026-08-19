.suggestion_abort <- function(message) {
  stop(message, call. = FALSE)
}

.suggestion_positive_limit <- function(limit) {
  if (length(limit) != 1L || is.na(limit) || !is.numeric(limit) ||
      limit < 1 || limit != as.integer(limit)) {
    .suggestion_abort("`limit` must be a positive integer.")
  }
  as.integer(limit)
}

new_suggestion <- function(id, label, value = label) {
  .port_assert_non_empty_string(id, "id")
  .port_assert_non_empty_string(label, "label")
  .port_assert_non_empty_string(value, "value")
  structure(
    list(id = id, label = label, value = value),
    class = c("suggestion", "excifinder_application_dto")
  )
}

is_suggestion <- function(x) {
  inherits(x, "suggestion")
}

new_excipient_suggestion_service <- function(
    product_source,
    composition_source) {
  if (!is_product_source_port(product_source)) {
    .suggestion_abort("`product_source` must be a product source port.")
  }
  if (!is_composition_source_port(composition_source)) {
    .suggestion_abort("`composition_source` must be a composition source port.")
  }

  suggest_excipients_for_active_ingredient <- function(
      active_ingredient,
      limit = 15L) {
    .port_assert_non_empty_string(active_ingredient, "active_ingredient")
    limit <- .suggestion_positive_limit(limit)

    products <- tryCatch(
      product_source$find_products_by_active_ingredient(
        active_ingredient,
        filters = list(authorized = TRUE, marketed = TRUE)
      ),
      error = function(error) list()
    )
    if (!is.list(products) || length(products) == 0L) {
      return(list())
    }

    names_by_key <- list()
    for (product in products) {
      if (!inherits(product, "medicinal_product")) next
      formulations <- tryCatch(
        product_source$get_formulations(medicinal_product_id(product)),
        error = function(error) list()
      )
      if (!is.list(formulations)) next

      for (formulation in formulations) {
        if (!inherits(formulation, "formulation")) next
        snapshot <- tryCatch(
          composition_source$get_composition_snapshot(formulation$id),
          error = function(error) new_port_absent("source_error")
        )
        if (!is_source_composition_snapshot(snapshot)) next

        for (entry in snapshot$entries) {
          name <- entry$name
          key <- normalize_excipient_text(name)
          if (!nzchar(key) || !is.null(names_by_key[[key]])) next
          names_by_key[[key]] <- name
          if (length(names_by_key) >= limit) break
        }
        if (length(names_by_key) >= limit) break
      }
      if (length(names_by_key) >= limit) break
    }

    unname(lapply(seq_along(names_by_key), function(index) {
      name <- names_by_key[[index]]
      new_suggestion(
        id = paste0("structured-excipient-", index),
        label = name,
        value = name
      )
    }))
  }

  structure(
    list(suggest_excipients_for_active_ingredient =
      suggest_excipients_for_active_ingredient),
    class = "excipient_suggestion_service"
  )
}
