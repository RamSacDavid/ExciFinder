test_that("medicinal products have stable authority-scoped identity", {
  product <- domain_env$new_medicinal_product(
    authority = "AEMPS",
    registration_number = "12345",
    name = "Example product",
    active_ingredients = "example ingredient",
    marketing_authorisation_holder = "Example holder",
    marketing_status = "authorised"
  )

  expect_s3_class(product, "medicinal_product")
  expect_equal(domain_env$medicinal_product_id(product), "AEMPS:12345")
  expect_error(
    domain_env$new_medicinal_product("AEMPS", "", "Example product"),
    "registration_number"
  )
})

test_that("formulation identity is opaque and supplied by the caller", {
  formulation <- domain_env$new_formulation(
    id = "opaque-formulation-id",
    medicinal_product_id = "AEMPS:12345",
    pharmaceutical_form = "tablet",
    route = "oral",
    strength = "10 mg"
  )

  expect_s3_class(formulation, "formulation")
  expect_equal(formulation$id, "opaque-formulation-id")
  expect_equal(formulation$medicinal_product_id, "AEMPS:12345")
})

test_that("presentations are distinct from formulations", {
  presentation <- domain_env$new_presentation(
    authority = "AEMPS",
    national_code = "654321",
    formulation_id = "opaque-formulation-id",
    description = "28 tablets",
    marketing_status = "marketed"
  )

  expect_s3_class(presentation, "presentation")
  expect_equal(domain_env$presentation_id(presentation), "AEMPS:654321")
  expect_equal(presentation$formulation_id, "opaque-formulation-id")
  expect_error(
    domain_env$new_presentation("AEMPS", "654321", "", "28 tablets"),
    "formulation_id"
  )
})
