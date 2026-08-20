test_that("CIMA active ingredient adapter maps maestras without leaking raw fields", {
  recording <- new_recording_cima_transport(function(url, query) {
    list(
      status = 200L,
      payload = list(resultados = list(
        list(id = 10L, nombre = "PARACETAMOL", internal = "raw"),
        list(codigo = "20", nombre = "PARACETAMOL / CODEÍNA"),
        list(id = 30L, nombre = "OTHER")
      ))
    )
  })
  source <- cima_adapter_env$new_cima_active_ingredient_suggestion_source(
    recording$transport,
    "https://test.invalid/rest/"
  )

  suggestions <- source$suggest_active_ingredients("para", 2L)
  call <- recording$calls()[[1]]

  expect_identical(call$url, "https://test.invalid/rest/maestras")
  expect_identical(call$query, list(maestra = 1L, nombre = "para"))
  expect_length(suggestions, 2L)
  expect_named(suggestions[[1]], c("id", "label", "value"))
  expect_identical(suggestions[[1]]$id, "10")
  expect_identical(suggestions[[1]]$value, "PARACETAMOL")
  expect_false("internal" %in% names(suggestions[[1]]))
})

test_that("CIMA suggestion adapter handles empty, 404, and transport errors", {
  empty <- cima_adapter_env$new_cima_active_ingredient_suggestion_source(
    function(url, query) list(status = 200L, payload = list(resultados = list()))
  )
  missing <- cima_adapter_env$new_cima_active_ingredient_suggestion_source(
    function(url, query) list(status = 404L, payload = NULL)
  )
  failed <- cima_adapter_env$new_cima_active_ingredient_suggestion_source(
    function(url, query) stop("controlled transport error")
  )

  expect_length(empty$suggest_active_ingredients("pa", 15L), 0L)
  expect_length(missing$suggest_active_ingredients("pa", 15L), 0L)
  expect_error(
    failed$suggest_active_ingredients("pa", 15L),
    "controlled transport error"
  )
})

test_that("CIMA suggestion default transport shares retry and User-Agent policy", {
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

  result <- cima_adapter_env$.cima_suggestion_default_transport(
    "https://test.invalid/maestras",
    list(maestra = 1L, nombre = "pa")
  )
  requests <- Filter(function(value) inherits(value, "request"), captured)

  expect_identical(result$status, 200L)
  expect_identical(captured$times, 3L)
  expect_true(any(vapply(requests, function(request) {
    identical(request$options$timeout_ms, 15000)
  }, logical(1))))
  expect_true(any(vapply(requests, function(request) {
    identical(
      request$options$useragent,
      paste0("ExciFinder/", cima_adapter_env$excifinder_version())
    )
  }, logical(1))))
})

test_that("short-lived memoization reduces six identical detail requests to one", {
  make_client <- function(counter) structure(
    list(
      find_medicines_page = function(...) list(),
      find_all_medicines = function(...) list(),
      get_medicine = function(registration_number) {
        counter$count <- counter$count + 1L
        list(nregistro = registration_number, nombre = "Product")
      }
    ),
    class = "cima_client"
  )

  before_counter <- new.env(parent = emptyenv())
  before_counter$count <- 0L
  before <- make_client(before_counter)
  before_results <- lapply(seq_len(6L), function(index) before$get_medicine("100"))

  after_counter <- new.env(parent = emptyenv())
  after_counter$count <- 0L
  memoized <- cima_adapter_env$new_memoized_cima_client(make_client(after_counter))
  after_results <- cima_adapter_env$with_cima_client_cache_scope(
    memoized,
    function() lapply(seq_len(6L), function(index) memoized$get_medicine("100"))
  )

  expect_identical(before_counter$count, 6L)
  expect_identical(after_counter$count, 1L)
  expect_identical(before_results, after_results)
  memoized$get_medicine("100")
  expect_identical(after_counter$count, 2L)
})
