.cima_mapper_abort <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("cima_mapper_error", "error", "condition")
  ))
}

.cima_raw_value_present <- function(value) {
  !is.null(value) && length(value) > 0L && !all(is.na(value))
}

.cima_required_text <- function(value, name) {
  if (!.cima_raw_value_present(value)) {
    .cima_mapper_abort(sprintf("Raw field `%s` is required.", name))
  }
  value <- as.character(value[[1]])
  if (!nzchar(trimws(value))) {
    .cima_mapper_abort(sprintf("Raw field `%s` cannot be empty.", name))
  }
  value
}

.cima_optional_text <- function(value) {
  if (!.cima_raw_value_present(value)) {
    return(NULL)
  }
  value <- as.character(value[[1]])
  if (!nzchar(trimws(value))) NULL else value
}

.cima_optional_logical <- function(value, name) {
  if (!.cima_raw_value_present(value)) {
    return(NULL)
  }
  value <- value[[1]]
  if (!is.logical(value) || is.na(value)) {
    .cima_mapper_abort(sprintf("Raw field `%s` must be logical.", name))
  }
  value
}

.cima_optional_integer <- function(value, name) {
  if (!.cima_raw_value_present(value)) {
    return(NULL)
  }
  value <- value[[1]]
  if (!is.numeric(value) || is.na(value) || value != as.integer(value)) {
    .cima_mapper_abort(sprintf("Raw field `%s` must be an integer.", name))
  }
  as.integer(value)
}

.cima_raw_collection <- function(value, name) {
  if (is.null(value)) {
    return(list())
  }
  if (!is.list(value)) {
    .cima_mapper_abort(sprintf("Raw field `%s` must be a list.", name))
  }
  unname(value)
}

map_cima_authorization_status <- function(raw_status) {
  if (is.null(raw_status) || !is.list(raw_status)) {
    return(NULL)
  }
  if (.cima_raw_value_present(raw_status$rev)) {
    return("revoked")
  }
  if (.cima_raw_value_present(raw_status$susp)) {
    return("suspended")
  }
  if (.cima_raw_value_present(raw_status$aut)) {
    return("authorized")
  }
  NULL
}

map_cima_active_ingredient_component <- function(raw_component) {
  if (!is.list(raw_component)) {
    .cima_mapper_abort("Raw active ingredient must be a list.")
  }
  new_active_ingredient_component(
    name = .cima_required_text(raw_component$nombre, "principiosActivos.nombre"),
    quantity = .cima_optional_text(raw_component$cantidad),
    unit = .cima_optional_text(raw_component$unidad),
    position = .cima_optional_integer(raw_component$orden, "principiosActivos.orden")
  )
}

map_cima_medicine_summary <- function(raw_medicine) {
  if (!is.list(raw_medicine)) {
    .cima_mapper_abort("Raw medicine summary must be a list.")
  }
  new_medicinal_product(
    authority = "AEMPS",
    registration_number = .cima_required_text(raw_medicine$nregistro, "nregistro"),
    name = .cima_required_text(raw_medicine$nombre, "nombre"),
    # `pactivos` is a delimited discovery summary, not structured composition.
    active_ingredients = list(),
    marketing_authorisation_holder = .cima_optional_text(raw_medicine$labtitular),
    authorization_status = map_cima_authorization_status(raw_medicine$estado),
    is_marketed = .cima_optional_logical(raw_medicine$comerc, "comerc")
  )
}

map_cima_medicinal_product <- function(raw_medicine) {
  if (!is.list(raw_medicine)) {
    .cima_mapper_abort("Raw detailed medicine must be a list.")
  }
  raw_components <- .cima_raw_collection(
    raw_medicine$principiosActivos,
    "principiosActivos"
  )
  new_medicinal_product(
    authority = "AEMPS",
    registration_number = .cima_required_text(raw_medicine$nregistro, "nregistro"),
    name = .cima_required_text(raw_medicine$nombre, "nombre"),
    active_ingredients = lapply(
      raw_components,
      map_cima_active_ingredient_component
    ),
    marketing_authorisation_holder = .cima_optional_text(raw_medicine$labtitular),
    authorization_status = map_cima_authorization_status(raw_medicine$estado),
    is_marketed = .cima_optional_logical(raw_medicine$comerc, "comerc")
  )
}

# This ID is adapter-local and provisional; it is not a regulatory identifier.
default_cima_formulation_id_factory <- function(product, raw_medicine) {
  paste0(medicinal_product_id(product), ":formulation:1")
}

map_cima_formulation <- function(
    raw_medicine,
    product,
    formulation_id_factory = default_cima_formulation_id_factory) {
  if (!is.function(formulation_id_factory)) {
    .cima_mapper_abort("`formulation_id_factory` must be a function.")
  }
  raw_routes <- .cima_raw_collection(
    raw_medicine$viasAdministracion,
    "viasAdministracion"
  )
  routes <- vapply(
    raw_routes,
    function(route) .cima_required_text(route$nombre, "viasAdministracion.nombre"),
    character(1)
  )
  pharmaceutical_form <- NULL
  if (!is.null(raw_medicine$formaFarmaceutica)) {
    pharmaceutical_form <- .cima_optional_text(raw_medicine$formaFarmaceutica$nombre)
  }

  new_formulation(
    id = formulation_id_factory(product, raw_medicine),
    medicinal_product_id = medicinal_product_id(product),
    pharmaceutical_form = pharmaceutical_form,
    routes = unname(routes),
    strength = .cima_optional_text(raw_medicine$dosis)
  )
}

map_cima_presentation <- function(raw_presentation, formulation_id) {
  if (!is.list(raw_presentation)) {
    .cima_mapper_abort("Raw presentation must be a list.")
  }
  new_presentation(
    authority = "AEMPS",
    national_code = .cima_required_text(raw_presentation$cn, "presentaciones.cn"),
    formulation_id = formulation_id,
    description = .cima_required_text(raw_presentation$nombre, "presentaciones.nombre"),
    authorization_status = map_cima_authorization_status(raw_presentation$estado),
    is_marketed = .cima_optional_logical(raw_presentation$comerc, "presentaciones.comerc")
  )
}

map_cima_presentations <- function(raw_medicine, formulation_id) {
  raw_presentations <- .cima_raw_collection(
    raw_medicine$presentaciones,
    "presentaciones"
  )
  lapply(
    raw_presentations,
    map_cima_presentation,
    formulation_id = formulation_id
  )
}

cima_structured_logical_source_key <- function(registration_number) {
  paste0("AEMPS:CIMA:medicine:", registration_number, ":structured")
}

cima_retrieval_snapshot_token <- function(retrieved_at) {
  if (!inherits(retrieved_at, "POSIXt") || length(retrieved_at) != 1L ||
      is.na(retrieved_at)) {
    .cima_mapper_abort("`retrieved_at` must be a single POSIX date-time.")
  }
  formatted <- format(
    as.POSIXct(retrieved_at),
    format = "%Y%m%dT%H%M%OS6Z",
    tz = "UTC",
    usetz = FALSE
  )
  gsub("[^0-9TZ]", "", formatted)
}

cima_structured_artifact_id <- function(registration_number, retrieved_at) {
  paste(
    cima_structured_logical_source_key(registration_number),
    cima_retrieval_snapshot_token(retrieved_at),
    sep = ":"
  )
}

map_cima_structured_source_artifact <- function(
    raw_medicine,
    formulation_id,
    retrieved_at) {
  registration_number <- .cima_required_text(raw_medicine$nregistro, "nregistro")
  new_source_artifact(
    id = cima_structured_artifact_id(registration_number, retrieved_at),
    source = "AEMPS:CIMA",
    subject_id = formulation_id,
    artifact_type = "structured_record",
    artifact_kind = "medicinal_product_record",
    retrieved_at = retrieved_at
  )
}

# A future stable content-hash policy may deduplicate snapshots without changing
# the meaning of evidence that references a concrete source artifact ID.

map_cima_source_excipient_entry <- function(
    raw_excipient,
    source_artifact_id,
    formulation_id) {
  if (!is.list(raw_excipient)) {
    .cima_mapper_abort("Raw excipient must be a list.")
  }
  raw_id <- raw_excipient$id
  if (is.null(raw_id)) {
    raw_id <- raw_excipient$Id
  }
  new_source_excipient_entry(
    source_artifact_id = source_artifact_id,
    source_record_id = .cima_optional_text(raw_id),
    subject_id = formulation_id,
    name = .cima_required_text(raw_excipient$nombre, "excipientes.nombre"),
    quantity = .cima_optional_text(raw_excipient$cantidad),
    unit = .cima_optional_text(raw_excipient$unidad),
    position = .cima_optional_integer(raw_excipient$orden, "excipientes.orden")
  )
}

map_cima_source_excipient_entries <- function(
    raw_medicine,
    source_artifact_id,
    formulation_id) {
  raw_excipients <- .cima_raw_collection(raw_medicine$excipientes, "excipientes")
  lapply(
    raw_excipients,
    map_cima_source_excipient_entry,
    source_artifact_id = source_artifact_id,
    formulation_id = formulation_id
  )
}
