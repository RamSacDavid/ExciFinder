make_small_excipient_taxonomy <- function() {
  application_env$new_excipient_taxonomy(
    version = "test-1",
    excipients = list(
      domain_env$new_excipient(
        id = "excipient-lactose",
        canonical_name = "lactosa",
        synonyms = c("lactosa monohidrato", "azúcar de leche"),
        language_variants = "lactose"
      ),
      domain_env$new_excipient(
        id = "excipient-benzyl-alcohol",
        canonical_name = "alcohol bencílico",
        synonyms = "benzyl alcohol",
        language_variants = "alcool benzylique"
      ),
      domain_env$new_excipient(
        id = "excipient-sorbitol",
        canonical_name = "sorbitol",
        e_codes = c("E-420", "E 420")
      )
    )
  )
}

test_that("excipient taxonomy validates version, objects, and unique IDs", {
  empty <- application_env$new_excipient_taxonomy("empty-1")
  lactose <- domain_env$new_excipient("excipient-lactose", "lactosa")

  taxonomy <- application_env$new_excipient_taxonomy("test-1", list(lactose))

  expect_s3_class(empty, "excipient_taxonomy")
  expect_s3_class(taxonomy, "excifinder_application_dto")
  expect_named(taxonomy, c("version", "excipients"))
  expect_error(application_env$new_excipient_taxonomy(""), "version")
  expect_error(
    application_env$new_excipient_taxonomy("test-1", list("not excipient")),
    "excipient"
  )
  expect_error(
    application_env$new_excipient_taxonomy("test-1", list(lactose, lactose)),
    "unique"
  )
})

test_that("safe normalization handles case, diacritics, spacing, and separators", {
  normalize <- application_env$normalize_excipient_text

  expect_identical(normalize(c("LACTOSA", "lactosa")), c("lactosa", "lactosa"))
  expect_identical(normalize(c("bencílico", "bencilico")), c("bencilico", "bencilico"))
  expect_false(identical(normalize("benzilico"), normalize("bencilico")))
  expect_identical(normalize(c("E-420", " E  420 ")), c("e 420", "e 420"))
  expect_identical(normalize("alcohol,  bencílico"), "alcohol bencilico")
  expect_identical(normalize("A+B? [C].*"), "a b c")
})

test_that("controlled terms preserve explicit provenance and deduplicate normalization", {
  sorbitol <- domain_env$new_excipient(
    "excipient-sorbitol",
    "sorbitol",
    synonyms = "glucitol",
    language_variants = "sorbitol",
    e_codes = c("E-420", "E 420")
  )

  terms <- application_env$excipient_controlled_terms(sorbitol)

  expect_named(terms, c("term", "normalized_term", "source"))
  expect_identical(terms$term, c("sorbitol", "glucitol", "E-420"))
  expect_identical(terms$source, c("canonical_name", "synonym", "e_code"))
  expect_identical(anyDuplicated(terms$normalized_term), 0L)
})

test_that("query resolution handles canonical, synonym, language, and E-code terms", {
  taxonomy <- make_small_excipient_taxonomy()

  canonical <- application_env$resolve_excipient_query(taxonomy, "LACTOSA")
  synonym <- application_env$resolve_excipient_query(taxonomy, "azucar de leche")
  language <- application_env$resolve_excipient_query(taxonomy, "lactose")
  e_code <- application_env$resolve_excipient_query(taxonomy, "E 420")

  expect_identical(canonical$status, "resolved")
  expect_identical(canonical$strategy, "taxonomy")
  expect_identical(canonical$candidates[[1]]$id, "excipient-lactose")
  expect_identical(synonym$status, "resolved")
  expect_identical(synonym$strategy, "taxonomy")
  expect_identical(synonym$matched_terms[[1]]$sources, "synonym")
  expect_identical(language$status, "resolved")
  expect_identical(language$matched_terms[[1]]$sources, "language_variant")
  expect_identical(e_code$status, "resolved")
  expect_identical(e_code$candidates[[1]]$id, "excipient-sorbitol")
})

test_that("query resolution reports not found and ambiguity without choosing", {
  shared_one <- domain_env$new_excipient(
    "excipient-one", "concepto uno", synonyms = "alias compartido"
  )
  shared_two <- domain_env$new_excipient(
    "excipient-two", "concepto dos", language_variants = "alias-compartido"
  )
  taxonomy <- application_env$new_excipient_taxonomy(
    "ambiguous-1",
    list(shared_one, shared_two)
  )

  ambiguous <- application_env$resolve_excipient_query(taxonomy, "Alias compartido")
  missing <- application_env$resolve_excipient_query(taxonomy, "sin concepto")

  expect_identical(ambiguous$status, "ambiguous")
  expect_identical(ambiguous$strategy, "taxonomy")
  expect_identical(
    vapply(ambiguous$candidates, `[[`, character(1), "id"),
    c("excipient-one", "excipient-two")
  )
  expect_length(ambiguous$matched_terms, 2L)
  expect_identical(missing$status, "not_found")
  expect_identical(missing$strategy, "taxonomy")
  expect_length(missing$candidates, 0L)
  expect_length(missing$matched_terms, 0L)
})

test_that("regex metacharacters in controlled queries remain ordinary input", {
  literal <- domain_env$new_excipient("literal", "A+B?")
  taxonomy <- application_env$new_excipient_taxonomy("literal-1", list(literal))

  resolved <- application_env$resolve_excipient_query(taxonomy, "A+B?")
  not_found <- application_env$resolve_excipient_query(taxonomy, "AAAB")

  expect_identical(resolved$status, "resolved")
  expect_identical(not_found$status, "not_found")
})
