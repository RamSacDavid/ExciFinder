new_source_document <- function(
    id,
    source,
    subject_id,
    document_type,
    url = NULL,
    document_date = NULL,
    retrieved_at = NULL,
    version = NULL,
    content_hash = NULL,
    language = NULL) {
  .domain_assert_non_empty_string(id, "id")
  .domain_assert_non_empty_string(source, "source")
  .domain_assert_non_empty_string(subject_id, "subject_id")
  .domain_assert_non_empty_string(document_type, "document_type")
  .domain_assert_optional_string(url, "url")
  .domain_assert_optional_string(version, "version")
  .domain_assert_optional_string(content_hash, "content_hash")
  .domain_assert_optional_string(language, "language")

  .new_domain_object(
    list(
      id = id,
      source = source,
      subject_id = subject_id,
      document_type = document_type,
      url = url,
      document_date = document_date,
      retrieved_at = retrieved_at,
      version = version,
      content_hash = content_hash,
      language = language
    ),
    "source_document"
  )
}
