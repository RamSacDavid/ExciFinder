new_excipient <- function(
    id,
    canonical_name,
    synonyms = character(),
    language_variants = character(),
    e_codes = character()) {
  .domain_assert_non_empty_string(id, "id")
  .domain_assert_non_empty_string(canonical_name, "canonical_name")

  .new_domain_object(
    list(
      id = id,
      canonical_name = canonical_name,
      synonyms = .domain_character_collection(synonyms, "synonyms"),
      language_variants = .domain_character_collection(
        language_variants,
        "language_variants"
      ),
      e_codes = .domain_character_collection(e_codes, "e_codes")
    ),
    "excipient"
  )
}
