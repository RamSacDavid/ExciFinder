test_that("active ingredient components preserve structured composition", {
  component <- domain_env$new_active_ingredient_component(
    name = "Example ingredient",
    quantity = "10.5",
    unit = "mg",
    position = 2L
  )

  expect_s3_class(component, "active_ingredient_component")
  expect_equal(component$name, "Example ingredient")
  expect_equal(component$quantity, "10.5")
  expect_equal(component$unit, "mg")
  expect_identical(component$position, 2L)
})

test_that("active ingredient components allow name-only values", {
  component <- domain_env$new_active_ingredient_component("Example ingredient")

  expect_null(component$quantity)
  expect_null(component$unit)
  expect_null(component$position)
  expect_error(domain_env$new_active_ingredient_component(""), "name")
  expect_error(
    domain_env$new_active_ingredient_component("Example", position = 1.5),
    "position"
  )
  expect_error(
    domain_env$new_active_ingredient_component("Example", position = NA_integer_),
    "position"
  )
})

test_that("medicinal products have stable authority-scoped identity", {
  product <- domain_env$new_medicinal_product(
    authority = "authority",
    registration_number = "12345",
    name = "Example product"
  )

  expect_s3_class(product, "medicinal_product")
  expect_equal(domain_env$medicinal_product_id(product), "authority:12345")
  expect_length(product$active_ingredients, 0L)
  expect_error(
    domain_env$new_medicinal_product("authority", "", "Example product"),
    "registration_number"
  )
})

test_that("medicinal products preserve ordered active ingredient components", {
  first <- domain_env$new_active_ingredient_component("First", position = 1L)
  second <- domain_env$new_active_ingredient_component("Second", position = 2L)
  one_component <- domain_env$new_medicinal_product(
    "authority", "12345", "Example product",
    active_ingredients = list(first)
  )
  two_components <- domain_env$new_medicinal_product(
    "authority", "12345", "Example product",
    active_ingredients = list(first, second)
  )

  expect_identical(one_component$active_ingredients, list(first))
  expect_identical(two_components$active_ingredients, list(first, second))
  expect_error(
    domain_env$new_medicinal_product(
      "authority", "12345", "Example product",
      active_ingredients = list("plain character value")
    ),
    "active_ingredient_component"
  )
})

test_that("product authorization and marketing state are independent", {
  marketed <- domain_env$new_medicinal_product(
    "authority", "12345", "Example product",
    authorization_status = "authorized",
    is_marketed = TRUE
  )
  not_marketed <- domain_env$new_medicinal_product(
    "authority", "12346", "Other product",
    authorization_status = "suspended",
    is_marketed = FALSE
  )
  unknown <- domain_env$new_medicinal_product(
    "authority", "12347", "Unknown state product"
  )

  expect_equal(marketed$authorization_status, "authorized")
  expect_true(marketed$is_marketed)
  expect_equal(not_marketed$authorization_status, "suspended")
  expect_false(not_marketed$is_marketed)
  expect_null(unknown$authorization_status)
  expect_null(unknown$is_marketed)
  expect_error(
    domain_env$new_medicinal_product(
      "authority", "12348", "Invalid state product", is_marketed = "yes"
    ),
    "is_marketed"
  )
  expect_error(
    domain_env$new_medicinal_product(
      "authority", "12349", "Invalid state product", is_marketed = NA
    ),
    "is_marketed"
  )
})

test_that("formulation identity is opaque and routes preserve cardinality", {
  without_routes <- domain_env$new_formulation(
    id = "opaque-formulation-id",
    medicinal_product_id = "authority:12345"
  )
  one_route <- domain_env$new_formulation(
    id = "opaque-formulation-id",
    medicinal_product_id = "authority:12345",
    pharmaceutical_form = "tablet",
    routes = "oral",
    strength = "10 mg"
  )
  multiple_routes <- domain_env$new_formulation(
    id = "opaque-formulation-id",
    medicinal_product_id = "authority:12345",
    routes = c("oral", "intravenous")
  )

  expect_s3_class(one_route, "formulation")
  expect_equal(one_route$id, "opaque-formulation-id")
  expect_length(without_routes$routes, 0L)
  expect_identical(one_route$routes, "oral")
  expect_identical(multiple_routes$routes, c("oral", "intravenous"))
  expect_error(
    domain_env$new_formulation(
      "opaque-formulation-id", "authority:12345", routes = c("oral", NA)
    ),
    "routes"
  )
})

test_that("presentations keep identity and independent state dimensions", {
  marketed <- domain_env$new_presentation(
    authority = "authority",
    national_code = "654321",
    formulation_id = "opaque-formulation-id",
    description = "28 tablets",
    authorization_status = "authorized",
    is_marketed = TRUE
  )
  not_marketed <- domain_env$new_presentation(
    "authority", "654322", "opaque-formulation-id", "56 tablets",
    authorization_status = "suspended",
    is_marketed = FALSE
  )
  unknown <- domain_env$new_presentation(
    "authority", "654323", "opaque-formulation-id", "14 tablets"
  )

  expect_s3_class(marketed, "presentation")
  expect_equal(domain_env$presentation_id(marketed), "authority:654321")
  expect_equal(marketed$formulation_id, "opaque-formulation-id")
  expect_true(marketed$is_marketed)
  expect_false(not_marketed$is_marketed)
  expect_null(unknown$authorization_status)
  expect_null(unknown$is_marketed)
  expect_error(
    domain_env$new_presentation("authority", "", "formulation", "Description"),
    "national_code"
  )
  expect_error(
    domain_env$new_presentation("authority", "654321", "", "Description"),
    "formulation_id"
  )
  expect_error(
    domain_env$new_presentation(
      "authority", "654321", "formulation", "Description", is_marketed = 1L
    ),
    "is_marketed"
  )
})
