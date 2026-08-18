.domain_abort <- function(message) {
  stop(message, call. = FALSE)
}

.domain_assert_non_empty_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .domain_abort(sprintf("`%s` must be a non-empty character string.", name))
  }
  invisible(value)
}

.domain_assert_optional_string <- function(value, name) {
  if (!is.null(value)) {
    .domain_assert_non_empty_string(value, name)
  }
  invisible(value)
}

.domain_character_collection <- function(value, name) {
  if (is.null(value)) {
    return(character())
  }
  if (!is.character(value) || anyNA(value)) {
    .domain_abort(sprintf("`%s` must be a character vector.", name))
  }
  unname(value)
}

.domain_object_collection <- function(value, class_name, name) {
  if (is.null(value)) {
    return(list())
  }
  if (!is.list(value) ||
      !all(vapply(value, inherits, logical(1), what = class_name))) {
    .domain_abort(sprintf(
      "`%s` must contain only `%s` objects.",
      name,
      class_name
    ))
  }
  unname(value)
}

.domain_assert_class <- function(value, class_name, name) {
  if (!inherits(value, class_name)) {
    .domain_abort(sprintf("`%s` must be a `%s` object.", name, class_name))
  }
  invisible(value)
}

.new_domain_object <- function(fields, class_name) {
  structure(fields, class = c(class_name, "excifinder_domain_object"))
}

new_medicinal_product <- function(
    authority,
    registration_number,
    name,
    active_ingredients = character(),
    marketing_authorisation_holder = NULL,
    marketing_status = NULL) {
  .domain_assert_non_empty_string(authority, "authority")
  .domain_assert_non_empty_string(registration_number, "registration_number")
  .domain_assert_non_empty_string(name, "name")
  active_ingredients <- .domain_character_collection(
    active_ingredients,
    "active_ingredients"
  )
  .domain_assert_optional_string(
    marketing_authorisation_holder,
    "marketing_authorisation_holder"
  )
  .domain_assert_optional_string(marketing_status, "marketing_status")

  .new_domain_object(
    list(
      authority = authority,
      registration_number = registration_number,
      name = name,
      active_ingredients = active_ingredients,
      marketing_authorisation_holder = marketing_authorisation_holder,
      marketing_status = marketing_status
    ),
    "medicinal_product"
  )
}

medicinal_product_id <- function(x) {
  .domain_assert_class(x, "medicinal_product", "x")
  paste(x$authority, x$registration_number, sep = ":")
}

new_formulation <- function(
    id,
    medicinal_product_id,
    pharmaceutical_form = NULL,
    route = NULL,
    strength = NULL) {
  .domain_assert_non_empty_string(id, "id")
  .domain_assert_non_empty_string(
    medicinal_product_id,
    "medicinal_product_id"
  )
  .domain_assert_optional_string(pharmaceutical_form, "pharmaceutical_form")
  .domain_assert_optional_string(route, "route")
  .domain_assert_optional_string(strength, "strength")

  .new_domain_object(
    list(
      id = id,
      medicinal_product_id = medicinal_product_id,
      pharmaceutical_form = pharmaceutical_form,
      route = route,
      strength = strength
    ),
    "formulation"
  )
}

new_presentation <- function(
    authority,
    national_code,
    formulation_id,
    description,
    marketing_status = NULL) {
  .domain_assert_non_empty_string(authority, "authority")
  .domain_assert_non_empty_string(national_code, "national_code")
  .domain_assert_non_empty_string(formulation_id, "formulation_id")
  .domain_assert_non_empty_string(description, "description")
  .domain_assert_optional_string(marketing_status, "marketing_status")

  .new_domain_object(
    list(
      authority = authority,
      national_code = national_code,
      formulation_id = formulation_id,
      description = description,
      marketing_status = marketing_status
    ),
    "presentation"
  )
}

presentation_id <- function(x) {
  .domain_assert_class(x, "presentation", "x")
  paste(x$authority, x$national_code, sep = ":")
}
