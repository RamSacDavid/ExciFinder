make_cima_source_test_client <- function(detail_fixture = "medicamento-detail.json") {
  recorder <- new_recording_cima_transport(function(url, query) {
    if (endsWith(url, "/medicamentos")) {
      return(read_cima_fixture("medicamentos-one-page.json"))
    }
    if (endsWith(url, "/medicamento")) {
      return(read_cima_fixture(detail_fixture))
    }
    stop("Unexpected adapter URL", call. = FALSE)
  })
  list(
    client = cima_adapter_env$new_cima_client(transport = recorder$transport),
    calls = recorder$calls
  )
}

test_that("CIMA ProductSourcePort exposes only canonical product objects", {
  setup <- make_cima_source_test_client()
  port <- cima_adapter_env$new_cima_product_source_port(setup$client)

  products <- port$find_products_by_active_ingredient(
    "ingredient",
    list(authorized = TRUE, marketed = TRUE)
  )
  product <- port$get_product("AEMPS:10001")

  expect_s3_class(port, "product_source_port")
  expect_length(products, 1L)
  expect_s3_class(products[[1]], "medicinal_product")
  expect_length(products[[1]]$active_ingredients, 0L)
  expect_s3_class(product, "medicinal_product")
  expect_length(product$active_ingredients, 2L)
  expect_false(any(c("nregistro", "pactivos", "comerc") %in% names(product)))
  expect_true(all(vapply(setup$calls(), function(call) {
    startsWith(call$url, "https://cima.aemps.es/cima/rest/")
  }, logical(1))))
})

test_that("CIMA ProductSourcePort maps formulation and presentations", {
  setup <- make_cima_source_test_client()
  port <- cima_adapter_env$new_cima_product_source_port(setup$client)

  formulations <- port$get_formulations("AEMPS:10001")
  presentations <- port$get_presentations(
    "AEMPS:10001:formulation:1",
    list()
  )

  expect_length(formulations, 1L)
  expect_s3_class(formulations[[1]], "formulation")
  expect_identical(
    formulations[[1]]$routes,
    c("Vía oral", "Vía intravenosa")
  )
  expect_length(presentations, 2L)
  expect_true(all(vapply(presentations, inherits, logical(1), "presentation")))
  expect_identical(presentations[[1]]$national_code, "600001")
  expect_identical(presentations[[2]]$national_code, "600002")
})

test_that("CIMA formulation identity strategy remains reversible and replaceable", {
  setup <- make_cima_source_test_client()
  strategy <- cima_adapter_env$new_cima_formulation_identity_strategy(
    make = function(product, raw_medicine) {
      paste0(cima_adapter_env$medicinal_product_id(product), ":local-form:primary")
    },
    product_id_from_formulation_id = function(formulation_id) {
      sub(":local-form:primary$", "", formulation_id)
    }
  )
  port <- cima_adapter_env$new_cima_product_source_port(
    setup$client,
    formulation_identity = strategy
  )

  formulations <- port$get_formulations("AEMPS:10001")
  presentations <- port$get_presentations(
    "AEMPS:10001:local-form:primary",
    list()
  )

  expect_identical(formulations[[1]]$id, "AEMPS:10001:local-form:primary")
  expect_length(presentations, 2L)
})

test_that("CIMA CompositionSourcePort returns entries without factual conclusions", {
  setup <- make_cima_source_test_client()
  retrieved_at <- as.POSIXct("2026-08-18 10:00:00", tz = "UTC")
  clock_calls <- 0L
  port <- cima_adapter_env$new_cima_composition_source_port(
    setup$client,
    retrieved_at = function() {
      clock_calls <<- clock_calls + 1L
      retrieved_at
    }
  )

  entries <- port$list_excipient_entries("AEMPS:10001:formulation:1")

  expect_s3_class(port, "composition_source_port")
  expect_length(entries, 2L)
  expected_artifact_id <- cima_adapter_env$cima_structured_artifact_id(
    "10001",
    retrieved_at
  )
  expect_true(all(vapply(entries, inherits, logical(1), "source_excipient_entry")))
  expect_true(all(vapply(entries, function(entry) {
    identical(entry$source_artifact_id, expected_artifact_id)
  }, logical(1))))
  expect_identical(clock_calls, 1L)
  expect_false(any(vapply(entries, inherits, logical(1), "excipient")))
  expect_false(any(vapply(entries, inherits, logical(1), "excipient_evidence")))
  expect_false(any(vapply(entries, inherits, logical(1), "excipient_assessment")))
  expect_false(any(c(
    "factual_conclusion", "identified", "not_identified", "indeterminate"
  ) %in% names(entries)))
})

test_that("empty structured entries do not imply absence", {
  setup <- make_cima_source_test_client("medicamento-no-excipients.json")
  port <- cima_adapter_env$new_cima_composition_source_port(setup$client)

  entries <- port$list_excipient_entries("AEMPS:10005:formulation:1")

  expect_type(entries, "list")
  expect_length(entries, 0L)
  expect_false(is.logical(entries))
  expect_false(inherits(entries, "excipient_assessment"))
})

test_that("CIMA source ports reject identifiers outside their namespace", {
  setup <- make_cima_source_test_client()
  product_port <- cima_adapter_env$new_cima_product_source_port(setup$client)
  composition_port <- cima_adapter_env$new_cima_composition_source_port(
    setup$client
  )

  expect_error(product_port$get_product("OTHER:10001"), class = "excifinder_port_error")
  expect_error(
    composition_port$list_excipient_entries("OTHER:formulation:1"),
    class = "excifinder_port_error"
  )
  expect_length(setup$calls(), 0L)
})
