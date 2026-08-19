test_that("CIMA document client negotiates plain, JSON, and HTML content", {
  contents <- list(
    `text/plain` = read_cima_text_fixture("document-section-61-plain.txt"),
    `application/json` = read_cima_text_fixture("document-section-61.json"),
    `text/html` = read_cima_text_fixture("document-section-61.html")
  )
  recorder <- new_recording_cima_document_transport(function(url, query, accept) {
    document_transport_response(contents[[accept]], accept)
  })
  client <- cima_adapter_env$new_cima_document_client(recorder$transport)

  responses <- lapply(names(contents), function(accept) {
    client$get_segmented_content(
      "20001",
      "summary_of_product_characteristics",
      section = "6.1",
      accept = accept
    )
  })

  expect_true(all(vapply(responses, inherits, logical(1), "cima_document_response")))
  expect_identical(vapply(responses, `[[`, character(1), "content_type"), names(contents))
  expect_true(all(vapply(recorder$calls(), function(call) {
    endsWith(call$url, "/docSegmentado/contenido/1") &&
      identical(call$query$seccion, "6.1")
  }, logical(1))))
})

test_that("CIMA document client omits section for full leaflet content", {
  recorder <- new_recording_cima_document_transport(function(url, query, accept) {
    document_transport_response(read_cima_text_fixture("document-full.txt"), accept)
  })
  client <- cima_adapter_env$new_cima_document_client(recorder$transport)

  response <- client$get_segmented_content(
    "20001",
    "package_leaflet",
    section = NULL,
    accept = "text/plain"
  )

  expect_s3_class(response, "cima_document_response")
  expect_false("seccion" %in% names(recorder$calls()[[1]]$query))
  expect_true(endsWith(recorder$calls()[[1]]$url, "/docSegmentado/contenido/2"))
})

test_that("CIMA document client distinguishes not found and technical errors", {
  not_found_fixture <- read_cima_fixture("document-section-not-found.json")
  missing_transport <- new_recording_cima_document_transport(
    function(url, query, accept) {
      document_transport_response(
        not_found_fixture$message,
        "application/json",
        not_found_fixture$status_code
      )
    }
  )
  failing_transport <- new_recording_cima_document_transport(
    function(url, query, accept) {
      document_transport_response("unavailable", "text/plain", 503L)
    }
  )
  missing_client <- cima_adapter_env$new_cima_document_client(
    missing_transport$transport
  )
  failing_client <- cima_adapter_env$new_cima_document_client(
    failing_transport$transport
  )

  missing <- missing_client$get_segmented_content(
    "20001", "summary_of_product_characteristics", "6.1"
  )

  expect_true(cima_adapter_env$is_cima_document_not_found(missing))
  expect_error(
    failing_client$get_segmented_content(
      "20001", "summary_of_product_characteristics", "6.1"
    ),
    class = "cima_document_client_error"
  )
})

test_that("CIMA document client marks unusable representations for fallback", {
  wrong_media <- cima_adapter_env$new_cima_document_client(
    function(url, query, accept) {
      document_transport_response("content", "application/octet-stream")
    }
  )
  empty_content <- cima_adapter_env$new_cima_document_client(
    function(url, query, accept) {
      document_transport_response("", accept)
    }
  )

  expect_true(cima_adapter_env$is_cima_document_unusable(
    wrong_media$get_segmented_content(
      "20001", "summary_of_product_characteristics", "6.1"
    )
  ))
  expect_true(cima_adapter_env$is_cima_document_unusable(
    empty_content$get_segmented_content(
      "20001", "summary_of_product_characteristics", section = NULL
    )
  ))
  expect_error(
    cima_adapter_env$new_cima_document_client(
      function(url, query, accept) stop("transport down", call. = FALSE)
    )$get_segmented_content(
      "20001", "summary_of_product_characteristics", "6.1"
    ),
    class = "cima_document_client_error"
  )
})
