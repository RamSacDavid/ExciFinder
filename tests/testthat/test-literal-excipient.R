make_literal_match_entry <- function(name) {
  application_env$new_source_excipient_entry(
    source_artifact_id = "artifact-literal-test",
    subject_id = "subject-literal-test",
    name = name
  )
}

test_that("literal excipients are deterministic ephemeral domain objects", {
  first <- application_env$new_literal_query_excipient("  Crospovidona  ")
  same <- application_env$new_literal_query_excipient("crospovidona")

  expect_s3_class(first, "excipient")
  expect_identical(first$canonical_name, "Crospovidona")
  expect_identical(first$synonyms, character())
  expect_identical(first$language_variants, character())
  expect_identical(first$e_codes, character())
  expect_true(startsWith(first$id, "literal:v1:"))
  expect_identical(first$id, same$id)
  expect_identical(
    application_env$literal_excipient_taxonomy_version(),
    "literal-v1"
  )
})

test_that("literal validation accepts safe punctuation and rejects unsafe inputs", {
  safe <- application_env$new_literal_query_excipient(
    "Ácido-alfa (tipo 2) + E-123"
  )
  limit <- application_env$literal_excipient_query_max_chars()

  expect_identical(safe$canonical_name, "Ácido-alfa (tipo 2) + E-123")
  expect_error(application_env$new_literal_query_excipient(""), "empty")
  expect_error(application_env$new_literal_query_excipient("   "), "whitespace")
  expect_error(
    application_env$new_literal_query_excipient(paste0("valid", "\u0007")),
    "control"
  )
  expect_error(
    application_env$new_literal_query_excipient(strrep("a", limit + 1L)),
    "200"
  )
  expect_s3_class(
    application_env$new_literal_query_excipient(strrep("a", limit)),
    "excipient"
  )
})

test_that("literal matching uses only the canonical query", {
  literal <- application_env$new_literal_query_excipient("Crospovidona")
  present <- application_env$match_excipient_entry(
    make_literal_match_entry("Crospovidona tipo A"),
    literal
  )
  absent <- application_env$match_excipient_entry(
    make_literal_match_entry("povidona"),
    literal
  )

  expect_identical(present$status, "matched")
  expect_identical(absent$status, "no_match")
})

test_that("literal matching never invents translations or spelling equivalence", {
  literal <- application_env$new_literal_query_excipient("alcohol bencílico")
  english <- application_env$match_excipient_entry(
    make_literal_match_entry("benzyl alcohol"),
    literal
  )
  different_spelling <- application_env$match_excipient_entry(
    make_literal_match_entry("alcohol benzílico"),
    literal
  )
  diacritic_only <- application_env$match_excipient_entry(
    make_literal_match_entry("ALCOHOL BENCILICO"),
    literal
  )

  expect_identical(english$status, "no_match")
  expect_identical(different_spelling$status, "no_match")
  expect_identical(diacritic_only$status, "matched")
})

test_that("regex metacharacters remain literal safe input", {
  literal <- application_env$new_literal_query_excipient("A+B? (tipo 1) [x].*")
  exact <- application_env$match_excipient_entry(
    make_literal_match_entry("Contiene A+B? (tipo 1) [x].*"),
    literal
  )
  regex_like <- application_env$match_excipient_entry(
    make_literal_match_entry("AAAB tipo 1 xxxx"),
    literal
  )

  expect_identical(exact$status, "matched")
  expect_identical(regex_like$status, "no_match")
})

