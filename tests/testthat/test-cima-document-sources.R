make_cima_document_source_setup <- function(
    raw_medicine,
    document_handler,
    retrieved_at = as.POSIXct("2026-08-19 10:00:00", tz = "UTC")) {
  metadata_transport <- new_recording_cima_transport(function(url, query) {
    if (endsWith(url, "/medicamento")) {
      return(raw_medicine)
    }
    stop("Unexpected metadata URL", call. = FALSE)
  })
  document_transport <- new_recording_cima_document_transport(document_handler)
  client <- cima_adapter_env$new_cima_client(metadata_transport$transport)
  document_client <- cima_adapter_env$new_cima_document_client(
    document_transport$transport
  )
  port <- cima_adapter_env$new_cima_document_source_artifact_port(
    client,
    document_client,
    retrieved_at = function() retrieved_at
  )
  list(
    port = port,
    metadata_calls = metadata_transport$calls,
    document_calls = document_transport$calls,
    retrieved_at = retrieved_at
  )
}

find_document_artifact <- function(port, product_id, artifact_kind) {
  artifacts <- port$list_source_artifacts(product_id, "document")
  matches <- Filter(
    function(artifact) identical(artifact$artifact_kind, artifact_kind),
    artifacts
  )
  stopifnot(length(matches) == 1L)
  matches[[1]]
}

test_that("CIMA SourceArtifactPort lists and gets canonical product documents", {
  raw <- read_cima_fixture("medicamento-documents.json")
  setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      stop("Document content should not be requested", call. = FALSE)
    }
  )

  artifacts <- setup$port$list_source_artifacts("AEMPS:20001")
  document_artifacts <- setup$port$list_source_artifacts(
    "AEMPS:20001",
    "document"
  )
  non_document_artifacts <- setup$port$list_source_artifacts(
    "AEMPS:20001",
    "structured_record"
  )
  artifact <- setup$port$get_source_artifact(artifacts[[2]]$id)

  expect_s3_class(setup$port, "source_artifact_port")
  expect_length(artifacts, 4L)
  expect_identical(
    vapply(document_artifacts, `[[`, character(1), "id"),
    vapply(artifacts, `[[`, character(1), "id")
  )
  expect_length(non_document_artifacts, 0L)
  expect_identical(artifact$id, artifacts[[2]]$id)
  expect_identical(artifact$subject_id, "AEMPS:20001")
  expect_true(all(vapply(artifacts, function(item) {
    identical(item$artifact_type, "document")
  }, logical(1))))
  expect_length(setup$document_calls(), 0L)
  expect_error(
    setup$port$list_source_artifacts("OTHER:20001"),
    class = "excifinder_port_error"
  )
})

test_that("CIMA SourceArtifactPort does not reconstruct disappeared versions", {
  original <- read_cima_fixture("medicamento-documents.json")
  old_artifact <- cima_adapter_env$map_cima_document_source_artifact(
    original$docs[[1]],
    "AEMPS:20001",
    as.POSIXct("2026-08-19", tz = "UTC")
  )
  current <- original
  current$docs[[1]]$fecha <- original$docs[[1]]$fecha + 86400000
  setup <- make_cima_document_source_setup(
    current,
    function(url, query, accept) {
      stop("Document content should not be requested", call. = FALSE)
    }
  )

  result <- setup$port$get_source_artifact(old_artifact$id)

  expect_true(cima_adapter_env$is_port_absent(result))
})

test_that("CIMA SourceArtifactPort retrieves complete multiblock section 6.1", {
  raw <- read_cima_fixture("medicamento-documents.json")
  multiblock <- read_cima_text_fixture("document-section-61-multiblock.txt")
  setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      document_transport_response(multiblock, "text/plain")
    }
  )
  artifact <- find_document_artifact(
    setup$port,
    "AEMPS:20001",
    "summary_of_product_characteristics"
  )

  content <- setup$port$get_source_content(artifact$id, "6.1")

  expect_s3_class(content, "source_content")
  expect_identical(content$source_artifact_id, artifact$id)
  expect_identical(content$section, "6.1")
  expect_identical(content$content_type, "text/plain")
  expect_identical(content$retrieval_method, "cima_segmented_plain")
  expect_identical(content$retrieved_at, setup$retrieved_at)
  expect_match(content$content, "Presentación/tipo A", fixed = TRUE)
  expect_match(content$content, "Presentación/tipo B", fixed = TRUE)
  expect_false(any(c(
    "identified", "not_identified", "indeterminate", "verification_coverage"
  ) %in% names(content)))
  expect_length(setup$document_calls(), 1L)
})

test_that("CIMA SourceArtifactPort retrieves complete segmented content", {
  raw <- read_cima_fixture("medicamento-documents.json")
  complete <- read_cima_text_fixture("document-full.txt")
  setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      document_transport_response(complete, "text/plain")
    }
  )
  artifact <- find_document_artifact(
    setup$port,
    "AEMPS:20001",
    "summary_of_product_characteristics"
  )

  content <- setup$port$get_source_content(artifact$id, section = NULL)

  expect_null(content$section)
  expect_identical(content$retrieval_method, "cima_segmented_plain")
  expect_false("seccion" %in% names(setup$document_calls()[[1]]$query))
})

test_that("CIMA SourceArtifactPort falls back from unusable plain to JSON", {
  raw <- read_cima_fixture("medicamento-documents.json")
  json_content <- read_cima_text_fixture("document-section-61.json")
  setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      if (identical(accept, "text/plain")) {
        return(document_transport_response("binary", "application/octet-stream"))
      }
      document_transport_response(json_content, accept)
    }
  )
  artifact <- find_document_artifact(
    setup$port,
    "AEMPS:20001",
    "summary_of_product_characteristics"
  )

  content <- setup$port$get_source_content(artifact$id, "6.1")

  expect_identical(content$retrieval_method, "cima_segmented_json")
  expect_identical(content$content_type, "text/html")
  expect_match(content$content, "Presentación A", fixed = TRUE)
  expect_identical(
    vapply(setup$document_calls(), `[[`, character(1), "accept"),
    c("text/plain", "application/json")
  )
})

test_that("CIMA SourceArtifactPort can reach the segmented HTML fallback", {
  raw <- read_cima_fixture("medicamento-documents.json")
  html_content <- read_cima_text_fixture("document-section-61.html")
  setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      if (!identical(accept, "text/html")) {
        return(document_transport_response("unusable", "application/octet-stream"))
      }
      document_transport_response(html_content, "text/html")
    }
  )
  artifact <- find_document_artifact(
    setup$port,
    "AEMPS:20001",
    "package_leaflet"
  )

  content <- setup$port$get_source_content(artifact$id, "6.1")

  expect_identical(content$retrieval_method, "cima_segmented_html")
  expect_identical(content$content_type, "text/html")
  expect_length(setup$document_calls(), 3L)
})

test_that("document 404 is absence while 5xx is a port error without fallback", {
  raw <- read_cima_fixture("medicamento-documents.json")
  missing_setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      document_transport_response("missing", "text/plain", 404L)
    }
  )
  failing_setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      document_transport_response("unavailable", "text/plain", 503L)
    }
  )
  missing_artifact <- find_document_artifact(
    missing_setup$port, "AEMPS:20001", "summary_of_product_characteristics"
  )
  failing_artifact <- find_document_artifact(
    failing_setup$port, "AEMPS:20001", "summary_of_product_characteristics"
  )

  missing <- missing_setup$port$get_source_content(missing_artifact$id, "6.1")

  expect_true(cima_adapter_env$is_port_absent(missing))
  expect_length(missing_setup$document_calls(), 1L)
  expect_error(
    failing_setup$port$get_source_content(failing_artifact$id, "6.1"),
    class = "excifinder_port_error"
  )
  expect_length(failing_setup$document_calls(), 1L)
})

test_that("existing unsupported documents and non-segmented content are errors", {
  raw <- read_cima_fixture("medicamento-documents.json")
  unsupported_setup <- make_cima_document_source_setup(
    raw,
    function(url, query, accept) {
      stop("Document content should not be requested", call. = FALSE)
    }
  )
  unsupported <- find_document_artifact(
    unsupported_setup$port,
    "AEMPS:20001",
    "public_assessment_report"
  )
  non_segmented_raw <- read_cima_fixture(
    "medicamento-document-no-segmentation.json"
  )
  non_segmented_setup <- make_cima_document_source_setup(
    non_segmented_raw,
    function(url, query, accept) {
      stop("Document content should not be requested", call. = FALSE)
    }
  )
  non_segmented <- find_document_artifact(
    non_segmented_setup$port,
    "AEMPS:20002",
    "summary_of_product_characteristics"
  )

  expect_error(
    unsupported_setup$port$get_source_content(unsupported$id, "6.1"),
    "unsupported",
    class = "excifinder_port_error"
  )
  expect_error(
    non_segmented_setup$port$get_source_content(non_segmented$id, "6.1"),
    "segmented content is unavailable",
    class = "excifinder_port_error"
  )
  expect_length(unsupported_setup$document_calls(), 0L)
  expect_length(non_segmented_setup$document_calls(), 0L)
})
