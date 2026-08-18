test_that("CIMA client validates construction", {
  client <- cima_adapter_env$new_cima_client(
    transport = function(url, query) list(resultados = list())
  )

  expect_s3_class(client, "cima_client")
  expect_error(
    cima_adapter_env$new_cima_client(transport = "not a function"),
    class = "cima_client_error"
  )
})

test_that("CIMA client translates canonical filters and retrieves one page", {
  recorder <- new_recording_cima_transport(function(url, query) {
    expect_true(endsWith(url, "/medicamentos"))
    read_cima_fixture("medicamentos-one-page.json")
  })
  client <- cima_adapter_env$new_cima_client(
    transport = recorder$transport,
    base_url = "https://test.invalid/rest"
  )

  page <- client$find_medicines_page(
    active_ingredient = "ingredient",
    authorized = TRUE,
    marketed = FALSE,
    page = 1L
  )
  call <- recorder$calls()[[1]]

  expect_length(page$resultados, 1L)
  expect_identical(call$query$practiv1, "ingredient")
  expect_identical(call$query$autorizados, 1L)
  expect_identical(call$query$comerc, 0L)
  expect_identical(call$query$pagina, 1L)
})

test_that("CIMA client composes two pages and stops on the last page", {
  recorder <- new_recording_cima_transport(function(url, query) {
    if (identical(query$pagina, 1L)) {
      return(read_cima_fixture("medicamentos-page-1.json"))
    }
    if (identical(query$pagina, 2L)) {
      return(read_cima_fixture("medicamentos-page-2.json"))
    }
    stop("Unexpected page request", call. = FALSE)
  })
  client <- cima_adapter_env$new_cima_client(transport = recorder$transport)

  medicines <- client$find_all_medicines("ingredient")

  expect_length(medicines, 3L)
  expect_identical(
    vapply(medicines, function(item) item$nregistro, character(1)),
    c("10001", "10002", "10003")
  )
  expect_length(recorder$calls(), 2L)
})

test_that("CIMA client returns an empty collection for zero results", {
  recorder <- new_recording_cima_transport(function(url, query) {
    read_cima_fixture("medicamentos-empty.json")
  })
  client <- cima_adapter_env$new_cima_client(transport = recorder$transport)

  medicines <- client$find_all_medicines("missing ingredient")

  expect_length(medicines, 0L)
  expect_length(recorder$calls(), 1L)
})

test_that("CIMA client retrieves detailed raw medicine", {
  recorder <- new_recording_cima_transport(function(url, query) {
    expect_true(endsWith(url, "/medicamento"))
    read_cima_fixture("medicamento-detail.json")
  })
  client <- cima_adapter_env$new_cima_client(transport = recorder$transport)

  medicine <- client$get_medicine("10001")
  call <- recorder$calls()[[1]]

  expect_identical(medicine$nregistro, "10001")
  expect_identical(call$query$nregistro, "10001")
})

test_that("CIMA client converts transport and response failures", {
  failing_client <- cima_adapter_env$new_cima_client(
    transport = function(url, query) stop("offline", call. = FALSE)
  )
  invalid_client <- cima_adapter_env$new_cima_client(
    transport = function(url, query) "invalid response"
  )
  invalid_page_client <- cima_adapter_env$new_cima_client(
    transport = function(url, query) list(unexpected = list())
  )

  expect_error(
    failing_client$get_medicine("10001"),
    "offline",
    class = "cima_client_error"
  )
  expect_error(
    invalid_client$get_medicine("10001"),
    "invalid response",
    class = "cima_client_error"
  )
  expect_error(
    invalid_page_client$find_medicines_page("ingredient"),
    "resultados",
    class = "cima_client_error"
  )
})

test_that("CIMA pagination has a finite safety limit", {
  client <- cima_adapter_env$new_cima_client(
    transport = function(url, query) {
      list(
        totalFilas = 100L,
        pagina = query$pagina,
        tamanioPagina = 1L,
        resultados = list(list(nregistro = as.character(query$pagina)))
      )
    }
  )

  expect_error(
    client$find_all_medicines("ingredient", max_pages = 2L),
    "safety limit",
    class = "cima_client_error"
  )
})
