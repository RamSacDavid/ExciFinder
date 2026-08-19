test_that("CIMA document mapper maps types 1 through 4 canonically", {
  raw <- read_cima_fixture("medicamento-documents.json")
  retrieved_at <- as.POSIXct("2026-08-19 10:00:00", tz = "UTC")
  artifacts <- cima_adapter_env$map_cima_document_source_artifacts(
    raw,
    "AEMPS:20001",
    retrieved_at
  )

  expect_length(artifacts, 4L)
  expect_identical(
    vapply(artifacts, `[[`, character(1), "artifact_kind"),
    c(
      "summary_of_product_characteristics",
      "package_leaflet",
      "public_assessment_report",
      "risk_management_plan"
    )
  )
  expect_true(all(vapply(artifacts, function(artifact) {
    identical(artifact$artifact_type, "document") &&
      identical(artifact$subject_id, "AEMPS:20001") &&
      identical(artifact$retrieved_at, retrieved_at)
  }, logical(1))))
  expect_identical(
    artifacts[[1]]$source_date,
    as.POSIXct("2024-01-01", tz = "UTC")
  )
  expect_false(any(c("tipo", "secc", "urlHtml", "raw_document_type") %in%
    names(artifacts[[1]])))
})

test_that("CIMA document mapper rejects unknown raw document types", {
  raw_document <- list(
    tipo = 99,
    url = "https://example.invalid/unknown.pdf",
    fecha = 1704067200000
  )

  expect_error(
    cima_adapter_env$map_cima_document_source_artifact(
      raw_document,
      "AEMPS:20001",
      as.POSIXct("2026-08-19", tz = "UTC")
    ),
    "Unknown CIMA document type",
    class = "cima_document_mapper_error"
  )
})

test_that("document artifact identity follows source version, not retrieval time", {
  raw <- read_cima_fixture("medicamento-documents.json")$docs[[1]]
  first_retrieval <- as.POSIXct("2026-08-19 10:00:00", tz = "UTC")
  later_retrieval <- first_retrieval + 3600
  first <- cima_adapter_env$map_cima_document_source_artifact(
    raw, "AEMPS:20001", first_retrieval
  )
  retrieved_later <- cima_adapter_env$map_cima_document_source_artifact(
    raw, "AEMPS:20001", later_retrieval
  )
  newer_raw <- raw
  newer_raw$fecha <- raw$fecha + 86400000
  newer_version <- cima_adapter_env$map_cima_document_source_artifact(
    newer_raw, "AEMPS:20001", later_retrieval
  )

  expect_identical(first$id, retrieved_later$id)
  expect_false(identical(first$retrieved_at, retrieved_later$retrieved_at))
  expect_false(identical(first$id, newer_version$id))
  expect_false(identical(first$source_date, newer_version$source_date))
})

test_that("document artifact IDs do not collide across fixture documents", {
  raw <- read_cima_fixture("medicamento-documents.json")
  artifacts <- cima_adapter_env$map_cima_document_source_artifacts(
    raw,
    "AEMPS:20001",
    as.POSIXct("2026-08-19", tz = "UTC")
  )

  expect_length(unique(vapply(artifacts, `[[`, character(1), "id")), 4L)
  expect_true(all(startsWith(
    vapply(artifacts, `[[`, character(1), "id"),
    "AEMPS:CIMA:medicine:20001:document:"
  )))
})
