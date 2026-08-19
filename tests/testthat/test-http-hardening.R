test_that("shared HTTP policy is bounded and retries only transient statuses", {
  policy <- cima_adapter_env$excifinder_http_policy()

  expect_identical(policy$timeout_seconds, 15)
  expect_identical(policy$max_attempts, 3L)
  expect_identical(policy$user_agent, "ExciFinder/1.0")
  expect_true(policy$pause_base > 0)
  expect_true(policy$pause_cap >= policy$pause_base)
  expect_identical(
    setdiff(400:599, policy$terminate_on),
    c(429L, 500L, 502L, 503L, 504L)
  )
  expect_true(all(c(400L, 401L, 403L, 404L, 501L) %in% policy$terminate_on))
})

test_that("catalog transport applies timeout, User-Agent, and retry bounds", {
  captured <- NULL
  response <- structure(
    list(
      status_code = 200L,
      content = charToRaw('{"resultados":[]}'),
      headers = list(),
      url = "https://test.invalid"
    ),
    class = "response"
  )
  testthat::local_mocked_bindings(
    RETRY = function(verb, url, ...) {
      captured <<- c(list(verb = verb, url = url), list(...))
      response
    },
    .package = "httr"
  )

  payload <- cima_adapter_env$.cima_default_transport(
    "https://test.invalid/medicamentos",
    list(practiv1 = "ingredient")
  )
  requests <- Filter(function(value) inherits(value, "request"), captured)

  expect_identical(captured$verb, "GET")
  expect_identical(captured$times, 3L)
  expect_identical(captured$pause_cap, 4)
  expect_true(captured$quiet)
  expect_identical(payload$resultados, list())
  expect_true(any(vapply(requests, function(request) {
    identical(request$options$timeout_ms, 15000)
  }, logical(1))))
  expect_true(any(vapply(requests, function(request) {
    identical(request$options$useragent, "ExciFinder/1.0")
  }, logical(1))))
})

test_that("document transport shares retry policy and preserves Accept", {
  captured <- NULL
  response <- structure(
    list(
      status_code = 200L,
      content = charToRaw("section content"),
      headers = list(`content-type` = "text/plain"),
      url = "https://test.invalid"
    ),
    class = "response"
  )
  testthat::local_mocked_bindings(
    RETRY = function(verb, url, ...) {
      captured <<- c(list(verb = verb, url = url), list(...))
      response
    },
    .package = "httr"
  )

  result <- cima_adapter_env$.cima_document_default_transport(
    "https://test.invalid/document",
    list(nregistro = "10001"),
    "text/plain"
  )
  requests <- Filter(function(value) inherits(value, "request"), captured)

  expect_identical(captured$times, 3L)
  expect_identical(result$status_code, 200L)
  expect_identical(result$content, "section content")
  expect_true(any(vapply(requests, function(request) {
    identical(unname(request$headers[["Accept"]]), "text/plain")
  }, logical(1))))
})
