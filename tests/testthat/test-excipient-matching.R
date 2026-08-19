make_matching_excipient <- function() {
  domain_env$new_excipient(
    id = "excipient-lactose",
    canonical_name = "lactosa",
    synonyms = c("azúcar de leche", "lactosa monohidrato"),
    language_variants = "lactose"
  )
}

make_matching_entry <- function(name, source_record_id = "entry-1") {
  application_env$new_source_excipient_entry(
    source_artifact_id = "artifact-1",
    source_record_id = source_record_id,
    subject_id = "formulation-1",
    name = name
  )
}

make_matching_content <- function(content, content_type = "text/plain") {
  application_env$new_source_content(
    source_artifact_id = "artifact-1",
    content = content,
    content_type = content_type,
    section = "6.1",
    retrieval_method = "fixture"
  )
}

test_that("structured entries match canonical and explicit synonym terms", {
  excipient <- make_matching_excipient()
  canonical_entry <- make_matching_entry("Lactosa monohidrato")
  synonym_entry <- make_matching_entry("Contiene azúcar de leche", "entry-2")

  canonical <- application_env$match_excipient_entry(canonical_entry, excipient)
  synonym <- application_env$match_excipient_entry(synonym_entry, excipient)

  expect_identical(canonical$status, "matched")
  expect_identical(canonical$matches[[1]]$matched_term, "lactosa")
  expect_identical(canonical$matches[[1]]$excerpt, "Lactosa monohidrato")
  expect_identical(canonical$matches[[1]]$location$source_record_id, "entry-1")
  expect_identical(synonym$status, "matched")
  expect_true(any(vapply(synonym$matches, function(candidate) {
    identical(candidate$matched_term, "azúcar de leche")
  }, logical(1))))
})

test_that("structured entry matching uses token boundaries", {
  lactose <- make_matching_excipient()
  oral <- domain_env$new_excipient("excipient-oral", "oral")

  expect_identical(
    application_env$match_excipient_entry(
      make_matching_entry("lactosado"), lactose
    )$status,
    "no_match"
  )
  expect_identical(
    application_env$match_excipient_entry(
      make_matching_entry("sorbitol"), oral
    )$status,
    "no_match"
  )
  expect_identical(
    application_env$match_excipient_entry(
      make_matching_entry("Alcohol bencílico anhidro"),
      domain_env$new_excipient("excipient-benzyl-alcohol", "alcohol bencílico")
    )$status,
    "matched"
  )
})

test_that("regex metacharacters are never interpreted as source patterns", {
  literal <- domain_env$new_excipient("literal", "A+B?")

  matched <- application_env$match_excipient_entry(
    make_matching_entry("Contiene A+B? hidratado"), literal
  )
  not_matched <- application_env$match_excipient_entry(
    make_matching_entry("Contiene AAAB hidratado"), literal
  )

  expect_identical(matched$status, "matched")
  expect_identical(not_matched$status, "no_match")
})

test_that("entry matcher returns local candidates without evidence or assessment", {
  result <- application_env$match_excipient_entry(
    make_matching_entry("Lactosa monohidrato"),
    make_matching_excipient()
  )
  candidate <- result$matches[[1]]

  expect_s3_class(candidate, "excipient_match_candidate")
  expect_named(
    candidate,
    c("excipient_id", "matched_term", "excerpt", "location", "method")
  )
  expect_false(any(c(
    "source_artifact_id", "subject_id", "evidence_id", "factual_conclusion",
    "risk"
  ) %in% names(candidate)))
  expect_false(inherits(candidate, "excipient_evidence"))
  expect_false(inherits(result, "excipient_assessment"))
})

test_that("plain content matching preserves original lines and locations", {
  content <- make_matching_content(paste(
    "Primera línea sin coincidencias",
    "Contiene LACTOSA monohidrato",
    "Otra línea con azúcar de leche",
    sep = "\n"
  ))

  result <- application_env$match_excipient_content(
    content,
    make_matching_excipient()
  )

  expect_identical(result$status, "matched")
  expect_true(length(result$matches) >= 2L)
  expect_true(any(vapply(result$matches, function(candidate) {
    identical(candidate$excerpt, "Contiene LACTOSA monohidrato") &&
      identical(candidate$location$line, 2L)
  }, logical(1))))
  expect_true(any(vapply(result$matches, function(candidate) {
    identical(candidate$excerpt, "Otra línea con azúcar de leche") &&
      identical(candidate$location$line, 3L)
  }, logical(1))))
})

test_that("plain content distinguishes analyzable no-match from unsupported HTML", {
  excipient <- make_matching_excipient()
  absent <- application_env$match_excipient_content(
    make_matching_content("Sacarosa y almidón"),
    excipient
  )
  empty <- application_env$match_excipient_content(
    make_matching_content(""),
    excipient
  )
  html <- application_env$match_excipient_content(
    make_matching_content("<p>Lactosa</p>", "text/html"),
    excipient
  )

  expect_identical(absent$status, "no_match")
  expect_identical(empty$status, "no_match")
  expect_identical(html$status, "unsupported_content")
  expect_length(absent$matches, 0L)
  expect_length(html$matches, 0L)
})

test_that("plain matching handles diacritics without implicit z-to-c substitution", {
  benzyl_alcohol <- domain_env$new_excipient(
    "excipient-benzyl-alcohol",
    "alcohol bencílico"
  )
  diacritic <- application_env$match_excipient_content(
    make_matching_content("Contiene alcohol bencilico"),
    benzyl_alcohol
  )
  spelling_change <- application_env$match_excipient_content(
    make_matching_content("Contiene alcohol benzilico"),
    benzyl_alcohol
  )

  expect_identical(diacritic$status, "matched")
  expect_identical(spelling_change$status, "no_match")
})

test_that("normalized duplicate controlled terms do not duplicate a line match", {
  sorbitol <- domain_env$new_excipient(
    "excipient-sorbitol",
    "sorbitol",
    e_codes = c("E-420", "E 420")
  )
  result <- application_env$match_excipient_content(
    make_matching_content("Incluye E 420"),
    sorbitol
  )

  expect_identical(result$status, "matched")
  expect_length(result$matches, 1L)
  expect_identical(result$matches[[1]]$matched_term, "E-420")
})

test_that("matcher no-match is not factual absence", {
  entry_result <- application_env$match_excipient_entry(
    make_matching_entry("sacarosa"),
    make_matching_excipient()
  )
  content_result <- application_env$match_excipient_content(
    make_matching_content("sacarosa"),
    make_matching_excipient()
  )

  expect_identical(entry_result$status, "no_match")
  expect_identical(content_result$status, "no_match")
  expect_false(any(c(
    "not_identified", "verified_absence", "verification_coverage",
    "factual_conclusion"
  ) %in% names(entry_result)))
  expect_false(any(c(
    "not_identified", "verified_absence", "verification_coverage",
    "factual_conclusion"
  ) %in% names(content_result)))
})
