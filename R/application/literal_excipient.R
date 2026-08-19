literal_excipient_query_max_chars <- function() {
  # Public literal input is bounded to keep application work predictable while
  # remaining far above normal excipient-name lengths.
  200L
}

literal_excipient_taxonomy_version <- function() {
  "literal-v1"
}

.literal_excipient_clean_query <- function(query) {
  if (!is.character(query) || length(query) != 1L || is.na(query)) {
    .excipient_application_abort(
      "Literal query must be a single, non-missing character string."
    )
  }
  if (stringi::stri_detect_regex(query, "\\p{Cc}")) {
    .excipient_application_abort("Literal query cannot contain control characters.")
  }
  cleaned <- stringi::stri_trim_both(query)
  if (!nzchar(cleaned)) {
    .excipient_application_abort("Literal query cannot be empty or whitespace only.")
  }
  if (stringi::stri_length(cleaned) > literal_excipient_query_max_chars()) {
    .excipient_application_abort(sprintf(
      "Literal query cannot exceed %d characters.",
      literal_excipient_query_max_chars()
    ))
  }
  cleaned
}

.literal_excipient_id <- function(cleaned_query) {
  normalized <- normalize_excipient_text(cleaned_query)
  if (!nzchar(normalized)) {
    normalized <- stringi::stri_trans_tolower(
      stringi::stri_trans_nfkc(cleaned_query),
      locale = "es"
    )
  }
  token <- paste(
    sprintf("%02x", as.integer(charToRaw(enc2utf8(normalized)))),
    collapse = ""
  )
  # This stable application ID identifies an ephemeral literal query, not a
  # permanent taxonomy concept or a regulatory identifier.
  paste0("literal:v1:", token)
}

new_literal_query_excipient <- function(query) {
  cleaned <- .literal_excipient_clean_query(query)
  new_excipient(
    id = .literal_excipient_id(cleaned),
    canonical_name = cleaned,
    synonyms = character(),
    language_variants = character(),
    e_codes = character()
  )
}

new_literal_excipient_resolution <- function(query, excipient) {
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  new_excipient_resolution(
    query = query,
    status = "resolved",
    strategy = "literal",
    candidates = list(excipient),
    matched_terms = list(list(
      excipient_id = excipient$id,
      terms = excipient$canonical_name,
      sources = "literal_query"
    ))
  )
}
