.excipient_application_abort <- function(message) {
  stop(message, call. = FALSE)
}

.excipient_assert_non_empty_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .excipient_application_abort(
      sprintf("`%s` must be a non-empty character string.", name)
    )
  }
  invisible(value)
}

normalize_excipient_text <- function(x) {
  if (!is.character(x)) {
    .excipient_application_abort("`x` must be a character vector.")
  }
  normalized <- stringi::stri_trans_nfkc(x)
  normalized <- stringi::stri_trans_tolower(normalized, locale = "es")
  normalized <- stringi::stri_trans_general(
    normalized,
    "NFD; [:Nonspacing Mark:] Remove; NFC"
  )
  normalized <- stringi::stri_replace_all_regex(
    normalized,
    "[\\p{P}\\p{S}\\p{Z}\\s]+",
    " "
  )
  stringi::stri_trim_both(normalized)
}

new_excipient_taxonomy <- function(version, excipients = list()) {
  .excipient_assert_non_empty_string(version, "version")
  if (!is.list(excipients)) {
    .excipient_application_abort("`excipients` must be a list of `excipient` objects.")
  }
  if (!all(vapply(excipients, inherits, logical(1), "excipient"))) {
    .excipient_application_abort("Every item in `excipients` must be an `excipient` object.")
  }
  ids <- vapply(excipients, `[[`, character(1), "id")
  if (anyDuplicated(ids)) {
    .excipient_application_abort("Excipient IDs must be unique within a taxonomy.")
  }

  structure(
    list(version = version, excipients = unname(excipients)),
    class = c("excipient_taxonomy", "excifinder_application_dto")
  )
}

is_excipient_taxonomy <- function(x) {
  inherits(x, "excipient_taxonomy")
}

excipient_controlled_terms <- function(excipient) {
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  terms <- c(
    excipient$canonical_name,
    excipient$synonyms,
    excipient$language_variants,
    excipient$e_codes
  )
  sources <- c(
    "canonical_name",
    rep("synonym", length(excipient$synonyms)),
    rep("language_variant", length(excipient$language_variants)),
    rep("e_code", length(excipient$e_codes))
  )
  normalized_terms <- normalize_excipient_text(terms)
  keep <- nzchar(normalized_terms) & !duplicated(normalized_terms)
  data.frame(
    term = terms[keep],
    normalized_term = normalized_terms[keep],
    source = sources[keep],
    stringsAsFactors = FALSE
  )
}

new_excipient_resolution <- function(
    query,
    status,
    candidates = list(),
    matched_terms = list(),
    strategy = "taxonomy") {
  if (!is.character(query) || length(query) != 1L || is.na(query)) {
    .excipient_application_abort("`query` must be a single, non-missing character string.")
  }
  statuses <- c("resolved", "ambiguous", "not_found")
  if (!is.character(status) || length(status) != 1L ||
      !status %in% statuses) {
    .excipient_application_abort(
      sprintf("`status` must be one of: %s.", paste(statuses, collapse = ", "))
    )
  }
  strategies <- c("taxonomy", "literal")
  if (!is.character(strategy) || length(strategy) != 1L ||
      is.na(strategy) || !strategy %in% strategies) {
    .excipient_application_abort(
      sprintf("`strategy` must be one of: %s.", paste(strategies, collapse = ", "))
    )
  }
  if (!identical(status, "not_found")) {
    .excipient_assert_non_empty_string(query, "query")
  }
  if (!is.list(candidates) ||
      !all(vapply(candidates, inherits, logical(1), "excipient"))) {
    .excipient_application_abort("`candidates` must contain only `excipient` objects.")
  }
  if (!is.list(matched_terms)) {
    .excipient_application_abort("`matched_terms` must be a list.")
  }
  if (identical(status, "resolved") && length(candidates) != 1L) {
    .excipient_application_abort("A resolved query must have exactly one candidate.")
  }
  if (identical(status, "ambiguous") && length(candidates) < 2L) {
    .excipient_application_abort("An ambiguous query must have at least two candidates.")
  }
  if (identical(status, "not_found") && length(candidates) != 0L) {
    .excipient_application_abort("A not-found query cannot have candidates.")
  }
  if (!identical(status, "resolved") && identical(strategy, "literal")) {
    .excipient_application_abort("Literal strategy requires a resolved query.")
  }

  structure(
    list(
      query = query,
      status = status,
      strategy = strategy,
      candidates = candidates,
      matched_terms = matched_terms
    ),
    class = c("excipient_resolution", "excifinder_application_dto")
  )
}

resolve_excipient_query <- function(taxonomy, query) {
  if (!is_excipient_taxonomy(taxonomy)) {
    .excipient_application_abort("`taxonomy` must be an `excipient_taxonomy` object.")
  }
  .excipient_assert_non_empty_string(query, "query")
  normalized_query <- normalize_excipient_text(query)
  if (!nzchar(normalized_query)) {
    .excipient_application_abort("`query` must contain searchable text.")
  }

  candidates <- list()
  matched_terms <- list()
  for (excipient in taxonomy$excipients) {
    terms <- excipient_controlled_terms(excipient)
    matches <- terms[terms$normalized_term == normalized_query, , drop = FALSE]
    if (nrow(matches) > 0L) {
      candidates[[length(candidates) + 1L]] <- excipient
      matched_terms[[length(matched_terms) + 1L]] <- list(
        excipient_id = excipient$id,
        terms = matches$term,
        sources = matches$source
      )
    }
  }

  status <- if (length(candidates) == 0L) {
    "not_found"
  } else if (length(candidates) == 1L) {
    "resolved"
  } else {
    "ambiguous"
  }
  new_excipient_resolution(query, status, candidates, matched_terms)
}
