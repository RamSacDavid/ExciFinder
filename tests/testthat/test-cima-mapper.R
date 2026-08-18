test_that("CIMA authorization status follows explicit precedence", {
  expect_identical(
    cima_adapter_env$map_cima_authorization_status(list(aut = 1)),
    "authorized"
  )
  expect_identical(
    cima_adapter_env$map_cima_authorization_status(list(aut = 1, susp = 2)),
    "suspended"
  )
  expect_identical(
    cima_adapter_env$map_cima_authorization_status(list(aut = 1, susp = 2, rev = 3)),
    "revoked"
  )
  expect_null(cima_adapter_env$map_cima_authorization_status(NULL))
  expect_null(cima_adapter_env$map_cima_authorization_status(list()))
})

test_that("detailed CIMA medicine maps to a canonical medicinal product", {
  raw <- read_cima_fixture("medicamento-detail.json")
  product <- cima_adapter_env$map_cima_medicinal_product(raw)

  expect_s3_class(product, "medicinal_product")
  expect_identical(product$authority, "AEMPS")
  expect_identical(product$registration_number, "10001")
  expect_identical(product$marketing_authorisation_holder, "EXAMPLE LAB")
  expect_identical(product$authorization_status, "authorized")
  expect_true(product$is_marketed)
  expect_length(product$active_ingredients, 2L)
  expect_identical(
    vapply(product$active_ingredients, function(item) item$name, character(1)),
    c("INGREDIENT ALPHA", "INGREDIENT BETA")
  )
  expect_identical(product$active_ingredients[[1]]$quantity, "10")
  expect_identical(product$active_ingredients[[2]]$position, 2L)
  expect_false(any(c("id", "codigo") %in% names(product$active_ingredients[[1]])))
})

test_that("summary medicine does not parse ambiguous active ingredient text", {
  raw <- read_cima_fixture("medicamentos-page-1.json")$resultados[[1]]
  raw$pactivos <- "Substance A, Substance B"
  product <- cima_adapter_env$map_cima_medicine_summary(raw)

  expect_s3_class(product, "medicinal_product")
  expect_length(product$active_ingredients, 0L)
  expect_identical(product$registration_number, "10001")
  expect_identical(product$name, "MEDICINE ALPHA 10 MG TABLETS")
  expect_identical(product$authorization_status, "authorized")
  expect_true(product$is_marketed)
  expect_false("nregistro" %in% names(product))
})

test_that("minimal CIMA medicine preserves absent optional fields", {
  raw <- read_cima_fixture("medicamento-detail-minimal.json")
  product <- cima_adapter_env$map_cima_medicinal_product(raw)

  expect_length(product$active_ingredients, 0L)
  expect_null(product$marketing_authorisation_holder)
  expect_null(product$authorization_status)
  expect_null(product$is_marketed)
})

test_that("CIMA formulation maps every route in source order", {
  raw <- read_cima_fixture("medicamento-detail.json")
  product <- cima_adapter_env$map_cima_medicinal_product(raw)
  formulation <- cima_adapter_env$map_cima_formulation(raw, product)

  expect_s3_class(formulation, "formulation")
  expect_identical(formulation$id, "AEMPS:10001:formulation:1")
  expect_identical(formulation$medicinal_product_id, "AEMPS:10001")
  expect_identical(formulation$pharmaceutical_form, "Comprimido")
  expect_identical(formulation$routes, c("Vía oral", "Vía intravenosa"))
  expect_identical(formulation$strength, "10 mg/5 mg")
})

test_that("CIMA formulation preserves zero and one route cardinality", {
  minimal_raw <- read_cima_fixture("medicamento-detail-minimal.json")
  minimal_product <- cima_adapter_env$map_cima_medicinal_product(minimal_raw)
  no_routes <- cima_adapter_env$map_cima_formulation(minimal_raw, minimal_product)
  one_raw <- read_cima_fixture("medicamento-no-excipients.json")
  one_product <- cima_adapter_env$map_cima_medicinal_product(one_raw)
  one_route <- cima_adapter_env$map_cima_formulation(one_raw, one_product)

  expect_length(no_routes$routes, 0L)
  expect_identical(one_route$routes, "Vía oral")
})

test_that("CIMA formulation ID factory is replaceable", {
  raw <- read_cima_fixture("medicamento-detail.json")
  product <- cima_adapter_env$map_cima_medicinal_product(raw)
  alternative <- function(product, raw_medicine) "adapter-local-alternative"

  formulation <- cima_adapter_env$map_cima_formulation(
    raw,
    product,
    formulation_id_factory = alternative
  )

  expect_identical(formulation$id, "adapter-local-alternative")
})

test_that("CIMA presentations remain distinct and share current formulation", {
  raw <- read_cima_fixture("medicamento-detail.json")
  product <- cima_adapter_env$map_cima_medicinal_product(raw)
  formulation <- cima_adapter_env$map_cima_formulation(raw, product)
  presentations <- cima_adapter_env$map_cima_presentations(raw, formulation$id)

  expect_length(presentations, 2L)
  expect_true(all(vapply(presentations, inherits, logical(1), "presentation")))
  expect_identical(
    vapply(presentations, function(item) item$national_code, character(1)),
    c("600001", "600002")
  )
  expect_true(all(vapply(
    presentations,
    function(item) identical(item$formulation_id, formulation$id),
    logical(1)
  )))
  expect_true(presentations[[1]]$is_marketed)
  expect_identical(presentations[[2]]$authorization_status, "suspended")
  expect_false(presentations[[2]]$is_marketed)
})

test_that("CIMA structured artifact identifies a deterministic snapshot", {
  raw <- read_cima_fixture("medicamento-detail.json")
  retrieved_at <- as.POSIXct("2026-08-18 10:00:00", tz = "UTC")
  same_snapshot <- cima_adapter_env$map_cima_structured_source_artifact(
    raw,
    "AEMPS:10001:formulation:1",
    retrieved_at
  )
  repeated_snapshot <- cima_adapter_env$map_cima_structured_source_artifact(
    raw,
    "AEMPS:10001:formulation:1",
    retrieved_at
  )
  later_snapshot <- cima_adapter_env$map_cima_structured_source_artifact(
    raw,
    "AEMPS:10001:formulation:1",
    retrieved_at + 1
  )

  expect_s3_class(same_snapshot, "source_artifact")
  expect_identical(same_snapshot$id, repeated_snapshot$id)
  expect_false(identical(same_snapshot$id, later_snapshot$id))
  expect_true(startsWith(
    same_snapshot$id,
    "AEMPS:CIMA:medicine:10001:structured:"
  ))
  expect_identical(same_snapshot$artifact_type, "structured_record")
  expect_identical(same_snapshot$subject_id, "AEMPS:10001:formulation:1")
  expect_identical(same_snapshot$retrieved_at, retrieved_at)
  expect_null(same_snapshot$source_date)
  expect_null(same_snapshot$content_hash)
  expect_null(same_snapshot$version)
})

test_that("CIMA structured entries share their snapshot artifact ID", {
  raw <- read_cima_fixture("medicamento-detail.json")
  retrieved_at <- as.POSIXct("2026-08-18 10:00:00", tz = "UTC")
  artifact <- cima_adapter_env$map_cima_structured_source_artifact(
    raw,
    "AEMPS:10001:formulation:1",
    retrieved_at
  )
  entries <- cima_adapter_env$map_cima_source_excipient_entries(
    raw,
    artifact$id,
    "AEMPS:10001:formulation:1"
  )

  expect_true(all(vapply(entries, function(entry) {
    identical(entry$source_artifact_id, artifact$id)
  }, logical(1))))
})

test_that("CIMA excipients map to source entries without canonical resolution", {
  raw <- read_cima_fixture("medicamento-detail.json")
  entries <- cima_adapter_env$map_cima_source_excipient_entries(
    raw,
    "AEMPS:CIMA:medicine:10001:structured:fixture-snapshot",
    "AEMPS:10001:formulation:1"
  )

  expect_length(entries, 2L)
  expect_true(all(vapply(entries, inherits, logical(1), "source_excipient_entry")))
  expect_identical(entries[[1]]$source_record_id, "701")
  expect_identical(entries[[1]]$name, "Lactosa monohidrato")
  expect_identical(entries[[1]]$quantity, "25")
  expect_identical(entries[[1]]$unit, "mg")
  expect_identical(entries[[1]]$position, 1L)
  expect_null(entries[[2]]$quantity)
  expect_null(entries[[2]]$unit)
  expect_false(any(vapply(entries, inherits, logical(1), "excipient")))
})

test_that("CIMA mapper supports zero structured excipients", {
  raw <- read_cima_fixture("medicamento-no-excipients.json")
  entries <- cima_adapter_env$map_cima_source_excipient_entries(
    raw,
    "AEMPS:CIMA:medicine:10005:structured:fixture-snapshot",
    "AEMPS:10005:formulation:1"
  )

  expect_length(entries, 0L)
})

test_that("CIMA excipient raw numeric ID supports documented capitalization", {
  entry <- cima_adapter_env$map_cima_source_excipient_entry(
    list(Id = 703L, nombre = "Source name"),
    "AEMPS:CIMA:medicine:10001:structured:fixture-snapshot",
    "AEMPS:10001:formulation:1"
  )

  expect_identical(entry$source_record_id, "703")
})
