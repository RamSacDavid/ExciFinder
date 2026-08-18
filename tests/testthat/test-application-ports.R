make_port_product <- function(registration_number = "10001") {
  active_ingredient <- domain_env$new_active_ingredient_component("ingredient")
  domain_env$new_medicinal_product(
    authority = "authority",
    registration_number = registration_number,
    name = paste("Product", registration_number),
    active_ingredients = list(active_ingredient)
  )
}

make_port_formulation <- function(product_id = "authority:10001") {
  domain_env$new_formulation(
    id = "formulation-001",
    medicinal_product_id = product_id,
    pharmaceutical_form = "tablet"
  )
}

make_port_presentation <- function(formulation_id = "formulation-001") {
  domain_env$new_presentation(
    authority = "authority",
    national_code = "20001",
    formulation_id = formulation_id,
    description = "28 tablets"
  )
}

make_product_source_fake <- function(product) {
  formulation <- make_port_formulation(domain_env$medicinal_product_id(product))
  presentation <- make_port_presentation(formulation$id)

  application_env$new_product_source_port(
    find_products_by_active_ingredient = function(active_ingredient, filters) {
      list(product)
    },
    get_product = function(product_id) {
      if (identical(product_id, domain_env$medicinal_product_id(product))) {
        product
      } else {
        application_env$new_port_absent()
      }
    },
    get_formulations = function(product_id) {
      if (identical(product_id, domain_env$medicinal_product_id(product))) {
        list(formulation)
      } else {
        list()
      }
    },
    get_presentations = function(formulation_id, filters) {
      if (identical(formulation_id, formulation$id)) {
        list(presentation)
      } else {
        list()
      }
    }
  )
}

make_artifact_source_fake <- function(artifact, content) {
  application_env$new_source_artifact_port(
    list_source_artifacts = function(subject_id, artifact_type = NULL) {
      if (identical(subject_id, artifact$subject_id) &&
          (is.null(artifact_type) || identical(artifact_type, artifact$artifact_type))) {
        list(artifact)
      } else {
        list()
      }
    },
    get_source_artifact = function(source_artifact_id) {
      if (identical(source_artifact_id, artifact$id)) {
        artifact
      } else {
        application_env$new_port_absent()
      }
    },
    get_source_content = function(source_artifact_id) {
      if (identical(source_artifact_id, artifact$id)) {
        content
      } else {
        application_env$new_port_absent()
      }
    }
  )
}

make_composition_source_fake <- function(entry_name) {
  entry <- application_env$new_source_excipient_entry(
    source_artifact_id = "artifact-structured-001",
    source_record_id = paste0("entry-", entry_name),
    subject_id = "formulation-001",
    name = entry_name
  )
  application_env$new_composition_source_port(
    list_excipient_entries = function(subject_id) {
      if (identical(subject_id, entry$subject_id)) list(entry) else list()
    }
  )
}

test_that("product source ports require their complete contract", {
  expect_error(
    application_env$new_product_source_port(
      get_product = function(product_id) NULL,
      get_formulations = function(product_id) list(),
      get_presentations = function(formulation_id, filters) list()
    ),
    "find_products_by_active_ingredient"
  )

  port <- make_product_source_fake(make_port_product())
  expect_s3_class(port, "product_source_port")
  expect_true(application_env$is_product_source_port(port))
  expect_named(
    port,
    c(
      "find_products_by_active_ingredient", "get_product", "get_formulations",
      "get_presentations"
    )
  )
})

test_that("different product source fakes are substitutable", {
  first <- make_product_source_fake(make_port_product("10001"))
  second <- make_product_source_fake(make_port_product("10002"))

  consumer <- function(product_source) {
    product_source$find_products_by_active_ingredient("ingredient", list())[[1]]
  }

  expect_identical(names(first), names(second))
  expect_s3_class(consumer(first), "medicinal_product")
  expect_s3_class(consumer(second), "medicinal_product")
  expect_false(identical(consumer(first), consumer(second)))
})

test_that("artifact source ports can return document artifacts", {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-document-001",
    source = "authority",
    subject_id = "formulation-001",
    artifact_type = "document"
  )
  content <- application_env$new_source_content(
    source_artifact_id = artifact$id,
    content = "Example document text",
    content_type = "text/plain"
  )
  port <- make_artifact_source_fake(artifact, content)

  expect_true(application_env$is_source_artifact_port(port))
  expect_s3_class(
    port$list_source_artifacts("formulation-001", "document")[[1]],
    "source_artifact"
  )
  expect_s3_class(port$get_source_content(artifact$id), "source_content")
  expect_true(application_env$is_port_absent(port$get_source_artifact("missing")))
})

test_that("artifact source ports can return structured artifacts", {
  artifact <- domain_env$new_source_artifact(
    id = "artifact-structured-001",
    source = "authority",
    subject_id = "formulation-001",
    artifact_type = "structured_record",
    version = "record-version-1"
  )
  content <- application_env$new_source_content(
    source_artifact_id = artifact$id,
    content = "normalized structured content",
    content_type = "application/structured-record"
  )
  port <- make_artifact_source_fake(artifact, content)

  expect_s3_class(port$get_source_artifact(artifact$id), "source_artifact")
  expect_equal(port$get_source_artifact(artifact$id)$artifact_type, "structured_record")
  expect_s3_class(port$get_source_content(artifact$id), "source_content")
})

test_that("composition source ports return source entries, not canonical concepts", {
  port <- make_composition_source_fake("source lactose name")
  entry <- port$list_excipient_entries("formulation-001")[[1]]

  expect_true(application_env$is_composition_source_port(port))
  expect_s3_class(entry, "source_excipient_entry")
  expect_false(inherits(entry, "excipient"))
  expect_false("excipient_id" %in% names(entry))
})

test_that("composition source fakes are substitutable", {
  first <- make_composition_source_fake("source name one")
  second <- make_composition_source_fake("source name two")
  caller <- function(source_port) {
    source_port$list_excipient_entries("formulation-001")[[1]]
  }

  expect_identical(names(first), names(second))
  expect_s3_class(caller(first), "source_excipient_entry")
  expect_s3_class(caller(second), "source_excipient_entry")
  expect_false(identical(caller(first), caller(second)))
})

test_that("catalog repository ports are independent from other repositories", {
  no_value <- function(...) application_env$new_port_absent()
  ignore_value <- function(...) invisible(NULL)
  repository <- application_env$new_catalog_repository_port(
    get_product = no_value,
    put_product = ignore_value,
    get_formulation = no_value,
    put_formulation = ignore_value,
    get_presentation = no_value,
    put_presentation = ignore_value
  )

  expect_true(application_env$is_catalog_repository_port(repository))
  expect_named(
    repository,
    c(
      "get_product", "put_product", "get_formulation", "put_formulation",
      "get_presentation", "put_presentation"
    )
  )
  expect_true(application_env$is_port_absent(repository$get_product("product-001")))
})

test_that("artifact repository ports are independent from other repositories", {
  no_value <- function(...) application_env$new_port_absent()
  ignore_value <- function(...) invisible(NULL)
  repository <- application_env$new_artifact_repository_port(
    get_artifact = no_value,
    put_artifact = ignore_value,
    get_source_content = no_value,
    put_source_content = ignore_value
  )

  expect_true(application_env$is_artifact_repository_port(repository))
  expect_named(
    repository,
    c(
      "get_artifact", "put_artifact", "get_source_content",
      "put_source_content"
    )
  )
  expect_true(application_env$is_port_absent(repository$get_artifact("artifact-001")))
})

test_that("assessment repository ports accept only their two operations", {
  stored_assessment <- NULL
  repository <- application_env$new_assessment_repository_port(
    get_assessment = function(key) {
      if (identical(key, "some-opaque-key") && !is.null(stored_assessment)) {
        stored_assessment
      } else {
        application_env$new_port_absent()
      }
    },
    put_assessment = function(key, assessment) {
      stored_assessment <<- assessment
      invisible(assessment)
    }
  )
  assessment <- domain_env$new_excipient_assessment(
    subject_id = "formulation-001",
    excipient_id = "excipient-001",
    factual_conclusion = "indeterminate",
    verification_coverage = "not_attempted",
    matcher_version = "matcher-1",
    taxonomy_version = "taxonomy-1"
  )

  expect_true(application_env$is_assessment_repository_port(repository))
  expect_named(repository, c("get_assessment", "put_assessment"))
  expect_true(application_env$is_port_absent(repository$get_assessment("some-opaque-key")))
  repository$put_assessment("some-opaque-key", assessment)
  expect_identical(repository$get_assessment("some-opaque-key"), assessment)
  expect_error(repository$get_assessment(""), "key")
})

test_that("assessment repository fakes are substitutable by opaque key", {
  first <- application_env$new_assessment_repository_port(
    get_assessment = function(key) application_env$new_port_absent(),
    put_assessment = function(key, assessment) invisible(assessment)
  )
  second <- application_env$new_assessment_repository_port(
    get_assessment = function(key) application_env$new_port_absent("not_found"),
    put_assessment = function(key, assessment) invisible(assessment)
  )
  caller <- function(repository, key) repository$get_assessment(key)

  expect_identical(names(first), names(second))
  expect_true(application_env$is_port_absent(caller(first, "some-opaque-key")))
  expect_true(application_env$is_port_absent(caller(second, "some-opaque-key")))
})

test_that("absence and operational failures have distinct contracts", {
  absent <- application_env$new_port_absent()
  failure <- application_env$new_port_error(
    "source unavailable",
    "product_source",
    "get_product"
  )

  expect_true(application_env$is_port_absent(absent))
  expect_false(is.null(absent))
  expect_s3_class(failure, "excifinder_port_error")
  expect_equal(failure$operation, "get_product")
})
