make_factual_excipient <- function(
    id = "excipient-lactose",
    canonical_name = "lactosa") {
  domain_env$new_excipient(id, canonical_name)
}

make_factual_structured_snapshot <- function(entry_names = character()) {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-structured-30001",
    source = "AEMPS:CIMA",
    subject_id = "AEMPS:30001:formulation:1",
    artifact_type = "structured_record",
    artifact_kind = "medicinal_product_record",
    retrieved_at = as.POSIXct("2026-08-19 09:00:00", tz = "UTC")
  )
  entries <- lapply(seq_along(entry_names), function(index) {
    application_env$new_source_excipient_entry(
      source_artifact_id = artifact$id,
      source_record_id = paste0("entry-", index),
      subject_id = artifact$subject_id,
      name = entry_names[[index]],
      position = index
    )
  })
  application_env$new_source_composition_snapshot(artifact, entries)
}

make_factual_smpc <- function(
    content,
    content_type = "text/plain",
    section = "6.1",
    artifact_kind = "summary_of_product_characteristics",
    artifact_type = "document",
    content_artifact_id = "artifact-smpc-30001") {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-smpc-30001",
    source = "AEMPS:CIMA",
    subject_id = "AEMPS:30001",
    artifact_type = artifact_type,
    artifact_kind = artifact_kind,
    retrieved_at = as.POSIXct("2026-08-19 09:30:00", tz = "UTC")
  )
  source_content <- application_env$new_source_content(
    source_artifact_id = content_artifact_id,
    content = content,
    content_type = content_type,
    section = section,
    retrieval_method = "cima_segmented_plain",
    retrieved_at = as.POSIXct("2026-08-19 10:00:00", tz = "UTC")
  )
  list(artifact = artifact, content = source_content)
}

assess_factual_fixture <- function(
    structured_snapshots = list(),
    smpc = NULL,
    structured_errors = list(),
    smpc_error = NULL,
    excipient = make_factual_excipient()) {
  if (is.null(structured_snapshots)) {
    structured_snapshots <- list()
  } else if (inherits(structured_snapshots, "source_composition_snapshot")) {
    structured_snapshots <- list(structured_snapshots)
  }
  if (!is.null(structured_errors) && !is.list(structured_errors)) {
    structured_errors <- list(structured_errors)
  }
  application_env$assess_excipient_from_retrieved_sources(
    subject_id = "AEMPS:30001",
    excipient = excipient,
    taxonomy_version = "taxonomy-test-1",
    matcher_version = "matcher-test-1",
    structured_snapshots = structured_snapshots,
    smpc_artifact = if (is.null(smpc)) NULL else smpc$artifact,
    smpc_content = if (is.null(smpc)) NULL else smpc$content,
    structured_errors = structured_errors,
    smpc_error = smpc_error
  )
}
