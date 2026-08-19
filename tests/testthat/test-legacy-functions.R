test_that("legacy normalizer remains independently callable", {
  app_env <- load_legacy_engine()

  expect_equal(
    app_env$normalizar("<b>LÁCTOSA</b>"),
    " lactosa "
  )
  # KNOWN LEGACY BEHAVIOUR — scheduled for deliberate replacement
  expect_equal(app_env$normalizar("Zinc"), "cinc")
})

test_that("legacy section 6.1 retrieval preserves positive and negative text", {
  app_env <- load_legacy_engine()

  with_mocked_get(app_env, make_cima_mock(), {
    text <- app_env$obtener_seccion_61_legacy("12345")
    expect_true(grepl("lactosa", text))
  })

  with_mocked_get(
    app_env,
    make_cima_mock(documento = "doc-61-absent.json"),
    {
      text <- app_env$obtener_seccion_61_legacy("12345")
      expect_false(grepl("lactosa", text))
    }
  )
})
