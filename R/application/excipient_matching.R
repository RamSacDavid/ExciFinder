.excipient_tokenize <- function(normalized_text) {
  if (is.na(normalized_text) || !nzchar(normalized_text)) {
    return(character())
  }
  strsplit(normalized_text, " ", fixed = TRUE)[[1]]
}

.excipient_term_present <- function(normalized_text, normalized_term) {
  text_tokens <- .excipient_tokenize(normalized_text)
  term_tokens <- .excipient_tokenize(normalized_term)
  if (length(term_tokens) == 0L || length(term_tokens) > length(text_tokens)) {
    return(FALSE)
  }
  starts <- seq_len(length(text_tokens) - length(term_tokens) + 1L)
  any(vapply(starts, function(start) {
    identical(
      text_tokens[start:(start + length(term_tokens) - 1L)],
      term_tokens
    )
  }, logical(1)))
}

new_excipient_match_candidate <- function(
    excipient_id,
    matched_term,
    excerpt,
    location = NULL,
    method = "controlled_term_exact") {
  .excipient_assert_non_empty_string(excipient_id, "excipient_id")
  .excipient_assert_non_empty_string(matched_term, "matched_term")
  if (!is.character(excerpt) || length(excerpt) != 1L || is.na(excerpt)) {
    .excipient_application_abort("`excerpt` must be a single character string.")
  }
  .excipient_assert_non_empty_string(method, "method")
  if (!is.null(location) && !is.list(location)) {
    .excipient_application_abort("`location` must be NULL or a list.")
  }

  structure(
    list(
      excipient_id = excipient_id,
      matched_term = matched_term,
      excerpt = excerpt,
      location = location,
      method = method
    ),
    class = c("excipient_match_candidate", "excifinder_application_dto")
  )
}

is_excipient_match_candidate <- function(x) {
  inherits(x, "excipient_match_candidate")
}

new_excipient_match_result <- function(status, matches = list()) {
  statuses <- c("matched", "no_match", "unsupported_content")
  if (!is.character(status) || length(status) != 1L || !status %in% statuses) {
    .excipient_application_abort(
      sprintf("`status` must be one of: %s.", paste(statuses, collapse = ", "))
    )
  }
  if (!is.list(matches) ||
      !all(vapply(matches, is_excipient_match_candidate, logical(1)))) {
    .excipient_application_abort(
      "`matches` must contain only `excipient_match_candidate` objects."
    )
  }
  if (identical(status, "matched") && length(matches) == 0L) {
    .excipient_application_abort("A matched result requires at least one candidate.")
  }
  if (!identical(status, "matched") && length(matches) != 0L) {
    .excipient_application_abort("Only a matched result may contain candidates.")
  }

  # A matcher-level no_match is local to the analyzed input. It is not a
  # factual not_identified assessment and does not establish verified absence.
  structure(
    list(status = status, matches = matches),
    class = c("excipient_match_result", "excifinder_application_dto")
  )
}

is_excipient_match_result <- function(x) {
  inherits(x, "excipient_match_result")
}

.excipient_matches_in_text <- function(text, excipient, excerpt, location) {
  normalized_text <- normalize_excipient_text(text)
  terms <- excipient_controlled_terms(excipient)
  found <- vapply(terms$normalized_term, function(term) {
    .excipient_term_present(normalized_text, term)
  }, logical(1))
  if (!any(found)) {
    return(list())
  }
  matched_terms <- terms[found, , drop = FALSE]
  lapply(seq_len(nrow(matched_terms)), function(index) {
    new_excipient_match_candidate(
      excipient_id = excipient$id,
      matched_term = matched_terms$term[[index]],
      excerpt = excerpt,
      location = location
    )
  })
}

match_excipient_entry <- function(entry, excipient) {
  if (!is_source_excipient_entry(entry)) {
    .excipient_application_abort(
      "`entry` must be a `source_excipient_entry` object."
    )
  }
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  location <- if (is.null(entry$source_record_id)) {
    NULL
  } else {
    list(source_record_id = entry$source_record_id)
  }
  matches <- .excipient_matches_in_text(
    entry$name,
    excipient,
    excerpt = entry$name,
    location = location
  )
  new_excipient_match_result(
    if (length(matches) > 0L) "matched" else "no_match",
    matches
  )
}

match_excipient_content <- function(source_content, excipient) {
  if (!is_source_content(source_content)) {
    .excipient_application_abort(
      "`source_content` must be a `source_content` object."
    )
  }
  if (!inherits(excipient, "excipient")) {
    .excipient_application_abort("`excipient` must be an `excipient` object.")
  }
  if (!identical(source_content$content_type, "text/plain")) {
    return(new_excipient_match_result("unsupported_content"))
  }

  lines <- strsplit(source_content$content, "\r\n|\n|\r", perl = TRUE)[[1]]
  if (length(lines) == 0L) {
    lines <- character()
  }
  matches <- list()
  for (line_number in seq_along(lines)) {
    original_line <- lines[[line_number]]
    if (!nzchar(normalize_excipient_text(original_line))) {
      next
    }
    line_matches <- .excipient_matches_in_text(
      original_line,
      excipient,
      excerpt = original_line,
      location = list(line = as.integer(line_number))
    )
    matches <- c(matches, line_matches)
  }
  new_excipient_match_result(
    if (length(matches) > 0L) "matched" else "no_match",
    matches
  )
}
