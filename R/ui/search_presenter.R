.presenter_assert_search_result <- function(search_result) {
  if (!inherits(search_result, "excipient_search_result")) {
    stop("`search_result` must be an `excipient_search_result`.", call. = FALSE)
  }
  invisible(search_result)
}

excifinder_excel_safe_text <- function(value) {
  if (!is.character(value)) {
    return(value)
  }
  dangerous <- !is.na(value) & grepl("^[=+@-]", value)
  value[dangerous] <- paste0("'", value[dangerous])
  value
}

.presenter_excel_safe_data <- function(data) {
  data[] <- lapply(data, excifinder_excel_safe_text)
  data
}

excifinder_status_label <- function(status) {
  labels <- c(
    identified = "Identificado",
    not_identified = "No identificado en fuentes verificadas",
    indeterminate = "No verificable",
    conflicting = "Fuentes discordantes"
  )
  if (!status %in% names(labels)) {
    return("Estado desconocido")
  }
  unname(labels[[status]])
}

excifinder_state_class <- function(status) {
  classes <- c(
    identified = "state-identified",
    not_identified = "state-not-identified",
    indeterminate = "state-indeterminate",
    conflicting = "state-conflicting"
  )
  if (!status %in% names(classes)) return("state-unknown")
  unname(classes[[status]])
}

split_search_results_by_conclusion <- function(search_result) {
  .presenter_assert_search_result(search_result)
  conclusions <- c(
    "identified", "not_identified", "indeterminate", "conflicting"
  )
  groups <- stats::setNames(vector("list", length(conclusions)), conclusions)
  for (product_result in search_result$results) {
    conclusion <- product_result$assessment$factual_conclusion
    if (conclusion %in% conclusions) {
      groups[[conclusion]][[length(groups[[conclusion]]) + 1L]] <- product_result
    }
  }
  groups
}

excifinder_coverage_label <- function(coverage) {
  labels <- c(
    complete = "Completa",
    partial = "Parcial",
    failed = "Fallida",
    not_attempted = "No realizada"
  )
  if (!coverage %in% names(labels)) {
    return("Desconocida")
  }
  unname(labels[[coverage]])
}

excifinder_resolution_label <- function(strategy) {
  labels <- c(
    literal = "Método de búsqueda: coincidencia literal normalizada",
    taxonomy = "Método de búsqueda: términos controlados"
  )
  if (!strategy %in% names(labels)) {
    return(NULL)
  }
  unname(labels[[strategy]])
}

.presenter_artifact_map <- function(product_result) {
  artifacts <- product_result$source_artifacts
  if (length(artifacts) == 0L) {
    return(list())
  }
  stats::setNames(artifacts, vapply(artifacts, `[[`, character(1), "id"))
}

.presenter_artifact_label <- function(artifact, section = NULL) {
  if (identical(artifact$artifact_type, "structured_record")) {
    return("CIMA estructurado")
  }
  if (identical(
      artifact$artifact_kind,
      "summary_of_product_characteristics")) {
    return(if (is.null(section)) {
      "Ficha técnica"
    } else {
      paste("Ficha técnica · sección", section)
    })
  }
  "Fuente oficial"
}

.presenter_source_labels <- function(product_result) {
  artifact_map <- .presenter_artifact_map(product_result)
  attempts <- Filter(function(attempt) {
    !is.null(attempt$source_artifact_id) &&
      attempt$source_artifact_id %in% names(artifact_map)
  }, product_result$assessment$attempts)
  if (length(attempts) == 0L) {
    return("Sin fuente verificada")
  }
  paste(unique(vapply(
    attempts,
    function(attempt) {
      .presenter_artifact_label(
        artifact_map[[attempt$source_artifact_id]],
        attempt$section
      )
    },
    character(1)
  )), collapse = " · ")
}

.presenter_evidence <- function(product_result) {
  assessment_evidence(product_result$assessment)
}

.presenter_evidence_display <- function(product_result) {
  evidence <- .presenter_evidence(product_result)
  if (length(evidence) == 0L) {
    return(switch(
      product_result$assessment$factual_conclusion,
      not_identified = "Sin coincidencias en fuentes verificadas",
      indeterminate = "Sin evidencia concluyente",
      "Sin evidencia textual"
    ))
  }
  first <- evidence[[1]]
  prefix <- if (is.null(first$section)) "" else paste0("§ ", first$section, " · ")
  excerpt <- paste0(prefix, first$excerpt)
  if (length(evidence) == 1L) excerpt else paste0(length(evidence), " coincidencias · ", excerpt)
}

.presenter_complete_evidence <- function(product_result) {
  evidence <- .presenter_evidence(product_result)
  if (length(evidence) == 0L) {
    return("")
  }
  paste(vapply(evidence, function(item) {
    prefix <- if (is.null(item$section)) "" else paste0("[", item$section, "] ")
    paste0(prefix, item$excerpt)
  }, character(1)), collapse = " ||| ")
}

.presenter_sections <- function(product_result) {
  evidence <- .presenter_evidence(product_result)
  sections <- unique(unlist(lapply(evidence, `[[`, "section"), use.names = FALSE))
  if (length(sections) == 0L) "" else paste(sections, collapse = " ||| ")
}

.presenter_smpc_artifacts <- function(product_result) {
  Filter(function(artifact) {
    identical(
      artifact$artifact_kind,
      "summary_of_product_characteristics"
    ) && is.character(artifact$url) && length(artifact$url) == 1L &&
      !is.na(artifact$url) && grepl("^https?://", artifact$url, ignore.case = TRUE)
  }, product_result$source_artifacts)
}

.presenter_smpc_links <- function(product_result) {
  artifacts <- .presenter_smpc_artifacts(product_result)
  if (length(artifacts) == 0L) {
    return("")
  }
  urls <- vapply(artifacts, `[[`, character(1), "url")
  artifacts <- artifacts[!duplicated(urls)]
  links <- lapply(seq_along(artifacts), function(index) {
    label <- if (length(artifacts) == 1L) {
      "Ficha técnica"
    } else {
      paste("Ficha técnica", index)
    }
    shiny::tags$a(
      href = artifacts[[index]]$url,
      target = "_blank",
      rel = "noopener noreferrer",
      label
    )
  })
  paste(vapply(links, as.character, character(1)), collapse = "<br>")
}

.presenter_smpc_urls <- function(product_result) {
  artifacts <- .presenter_smpc_artifacts(product_result)
  if (length(artifacts) == 0L) {
    return("")
  }
  paste(unique(vapply(artifacts, `[[`, character(1), "url")), collapse = " ||| ")
}

.presenter_formulation_values <- function(product_result, field) {
  values <- vapply(product_result$formulations, function(formulation) {
    value <- formulation[[field]]
    if (is.null(value)) "" else value
  }, character(1))
  values <- unique(values[nzchar(values)])
  paste(values, collapse = " · ")
}

.presenter_empty_table <- function() {
  data.frame(
    Estado = character(),
    Cobertura = character(),
    Medicamento = character(),
    `Forma farmacéutica` = character(),
    `Dosis / strength` = character(),
    `N.º registro` = character(),
    Fuente = character(),
    Evidencia = character(),
    `Ficha técnica` = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

present_search_table <- function(search_result, conclusion = NULL) {
  .presenter_assert_search_result(search_result)
  if (!identical(search_result$resolution$status, "resolved") ||
      length(search_result$results) == 0L) {
    return(.presenter_empty_table())
  }
  results <- search_result$results
  if (!is.null(conclusion)) {
    groups <- split_search_results_by_conclusion(search_result)
    if (!conclusion %in% names(groups)) {
      stop("Unknown factual conclusion.", call. = FALSE)
    }
    results <- groups[[conclusion]]
  }
  if (length(results) == 0L) return(.presenter_empty_table())
  rows <- lapply(results, function(product_result) {
    data.frame(
      Estado = excifinder_status_label(
        product_result$assessment$factual_conclusion
      ),
      Cobertura = excifinder_coverage_label(
        product_result$assessment$verification_coverage
      ),
      Medicamento = product_result$product$name,
      `Forma farmacéutica` = .presenter_formulation_values(
        product_result, "pharmaceutical_form"
      ),
      `Dosis / strength` = .presenter_formulation_values(
        product_result, "strength"
      ),
      `N.º registro` = product_result$product$registration_number,
      Fuente = .presenter_source_labels(product_result),
      Evidencia = .presenter_evidence_display(product_result),
      `Ficha técnica` = .presenter_smpc_links(product_result),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Group headers already communicate the factual conclusion. This view removes
# only the redundant visual column; the underlying presenter and export retain it.
present_grouped_search_table <- function(search_result, conclusion) {
  table <- present_search_table(search_result, conclusion)
  table[setdiff(names(table), "Estado")]
}

present_search_export <- function(search_result) {
  .presenter_assert_search_result(search_result)
  if (length(search_result$results) == 0L) {
    return(data.frame(
      Medicamento = character(), Forma_farmaceutica = character(),
      Dosis_strength = character(), Numero_registro = character(),
      Estado = character(), Cobertura = character(), Metodo_busqueda = character(),
      Fuentes = character(), Secciones = character(), Evidencias = character(),
      URL_Ficha_Tecnica = character(), stringsAsFactors = FALSE
    ))
  }
  method <- excifinder_resolution_label(search_result$resolution$strategy)
  rows <- lapply(search_result$results, function(product_result) {
    data.frame(
      Medicamento = product_result$product$name,
      Forma_farmaceutica = .presenter_formulation_values(
        product_result, "pharmaceutical_form"
      ),
      Dosis_strength = .presenter_formulation_values(
        product_result, "strength"
      ),
      Numero_registro = product_result$product$registration_number,
      Estado = excifinder_status_label(product_result$assessment$factual_conclusion),
      Cobertura = excifinder_coverage_label(product_result$assessment$verification_coverage),
      Metodo_busqueda = method,
      Fuentes = .presenter_source_labels(product_result),
      Secciones = .presenter_sections(product_result),
      Evidencias = .presenter_complete_evidence(product_result),
      URL_Ficha_Tecnica = .presenter_smpc_urls(product_result),
      stringsAsFactors = FALSE
    )
  })
  .presenter_excel_safe_data(do.call(rbind, rows))
}

present_partial_errors <- function(search_result) {
  .presenter_assert_search_result(search_result)
  if (length(search_result$errors) == 0L) {
    return(data.frame(
      Medicamento_ID = character(), Etapa = character(), Mensaje = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(search_result$errors, function(error) {
    data.frame(
      Medicamento_ID = if (is.null(error$subject_id)) "Búsqueda" else error$subject_id,
      Etapa = error$stage,
      Mensaje = error$message,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

present_search_messages <- function(search_result) {
  .presenter_assert_search_result(search_result)
  resolution <- search_result$resolution
  method <- if (identical(resolution$status, "resolved")) {
    excifinder_resolution_label(resolution$strategy)
  } else {
    NULL
  }
  if (identical(resolution$status, "ambiguous")) {
    candidates <- vapply(
      resolution$candidates,
      `[[`,
      character(1),
      "canonical_name"
    )
    return(list(
      method = NULL,
      primary = paste0(
        "El término coincide con más de un concepto de excipiente. Reformule la búsqueda.",
        if (length(candidates) == 0L) "" else paste0(" Candidatos: ", paste(candidates, collapse = ", "), ".")
      ),
      warning = NULL
    ))
  }
  invalid <- any(vapply(search_result$errors, function(error) {
    error$code %in% c("invalid_literal_query", "invalid_excipient_query")
  }, logical(1)))
  if (invalid) {
    return(list(
      method = NULL,
      primary = "La consulta de excipiente no es válida. Revise el término introducido.",
      warning = NULL
    ))
  }
  if (identical(resolution$status, "resolved") &&
      length(search_result$results) == 0L && length(search_result$errors) == 0L) {
    return(list(
      method = method,
      primary = "No se encontraron medicamentos autorizados y comercializados para el principio activo indicado.",
      warning = NULL
    ))
  }
  warning <- if (length(search_result$errors) > 0L &&
      length(search_result$results) > 0L) {
    "Algunos medicamentos o fuentes no pudieron verificarse completamente."
  } else if (length(search_result$errors) > 0L) {
    "No fue posible completar la búsqueda con las fuentes disponibles."
  } else {
    NULL
  }
  list(method = method, primary = NULL, warning = warning)
}
