new_excipient_evidence <- function(
    id,
    excipient_id,
    subject_id,
    source_artifact_id,
    matched_term,
    section = NULL,
    excerpt,
    method,
    location = NULL) {
  .domain_assert_non_empty_string(id, "id")
  .domain_assert_non_empty_string(excipient_id, "excipient_id")
  .domain_assert_non_empty_string(subject_id, "subject_id")
  .domain_assert_non_empty_string(source_artifact_id, "source_artifact_id")
  .domain_assert_non_empty_string(matched_term, "matched_term")
  .domain_assert_optional_string(section, "section")
  .domain_assert_non_empty_string(excerpt, "excerpt")
  .domain_assert_non_empty_string(method, "method")

  .new_domain_object(
    list(
      id = id,
      excipient_id = excipient_id,
      subject_id = subject_id,
      source_artifact_id = source_artifact_id,
      matched_term = matched_term,
      section = section,
      excerpt = excerpt,
      method = method,
      location = location
    ),
    "excipient_evidence"
  )
}
