source_artifact_types <- function() {
  c("structured_record", "document")
}

# artifact_type is the representation class; artifact_kind is the semantic role/content.
new_source_artifact <- function(
    id,
    source,
    subject_id,
    artifact_type,
    artifact_kind,
    url = NULL,
    source_date = NULL,
    retrieved_at = NULL,
    version = NULL,
    content_hash = NULL,
    language = NULL) {
  .domain_assert_non_empty_string(id, "id")
  .domain_assert_non_empty_string(source, "source")
  .domain_assert_non_empty_string(subject_id, "subject_id")
  .domain_assert_non_empty_string(artifact_type, "artifact_type")
  if (!artifact_type %in% source_artifact_types()) {
    .domain_abort(sprintf(
      "`artifact_type` must be one of: %s.",
      paste(source_artifact_types(), collapse = ", ")
    ))
  }
  .domain_assert_non_empty_string(artifact_kind, "artifact_kind")
  .domain_assert_optional_string(url, "url")
  .domain_assert_optional_string(version, "version")
  .domain_assert_optional_string(content_hash, "content_hash")
  .domain_assert_optional_string(language, "language")

  .new_domain_object(
    list(
      id = id,
      source = source,
      subject_id = subject_id,
      artifact_type = artifact_type,
      artifact_kind = artifact_kind,
      url = url,
      source_date = source_date,
      retrieved_at = retrieved_at,
      version = version,
      content_hash = content_hash,
      language = language
    ),
    "source_artifact"
  )
}
