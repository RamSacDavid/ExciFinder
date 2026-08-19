new_parallel_validation_case <- function(
    case_id,
    description,
    excipient_query,
    legacy_expected,
    new_expected,
    comparison_expectation,
    rationale,
    legacy_scenario,
    new_scenario,
    new_expected_coverage = NULL,
    new_expected_strategy = NULL,
    new_expected_result_count = 1L,
    new_expected_source_calls = NULL) {
  list(
    case_id = case_id,
    description = description,
    excipient_query = excipient_query,
    legacy_expected = legacy_expected,
    new_expected = new_expected,
    comparison_expectation = comparison_expectation,
    rationale = rationale,
    gold_expected = new_expected,
    legacy_scenario = legacy_scenario,
    new_scenario = new_scenario,
    new_expected_coverage = new_expected_coverage,
    new_expected_strategy = new_expected_strategy,
    new_expected_result_count = as.integer(new_expected_result_count),
    new_expected_source_calls = new_expected_source_calls
  )
}

parallel_validation_cases <- function() {
  multiblock <- list(fixture = "document-section-61-multiblock.txt")
  list(
    new_parallel_validation_case(
      "A", "Clear positive in complete section 6.1", "lactosa",
      "legacy_positive", "new_identified", "equivalent",
      "Both engines identify a direct product-level occurrence.",
      list(document_text = "Excipientes: lactosa monohidrato"),
      list(document_text = "Excipientes: lactosa monohidrato"),
      "complete", "taxonomy"
    ),
    new_parallel_validation_case(
      "B", "Clear negative in valid complete section 6.1", "lactosa",
      "legacy_negative", "new_not_identified", "equivalent",
      "Both engines report product-level absence after readable complete content.",
      list(document_text = "Excipientes: sacarosa"),
      list(structured_entries = list("sacarosa"), document_text = "Excipientes: sacarosa"),
      "complete", "taxonomy"
    ),
    new_parallel_validation_case(
      "C", "Technical failure of exhaustive verification", "lactosa",
      "legacy_negative", "new_indeterminate", "expected_improvement",
      "The new engine preserves non-verification instead of converting failure to absence.",
      list(document_status = 503L),
      list(document_mode = "failed"),
      "failed", "taxonomy"
    ),
    new_parallel_validation_case(
      "D", "Structured positive with unavailable section 6.1", "lactosa",
      "legacy_negative", "new_identified", "expected_improvement",
      "Official structured evidence establishes presence with partial coverage.",
      list(document_status = 503L),
      list(structured_entries = list("lactosa"), document_mode = "absent"),
      "partial", "taxonomy"
    ),
    new_parallel_validation_case(
      "E", "Structured negative with failed section 6.1", "lactosa",
      "legacy_negative", "new_indeterminate", "expected_improvement",
      "A non-exhaustive structured no-match cannot establish absence.",
      list(document_status = 503L),
      list(structured_entries = list("sacarosa"), document_mode = "failed"),
      "partial", "taxonomy"
    ),
    new_parallel_validation_case(
      "F", "Isolated legacy global z-to-c equivalence", "zeta",
      "legacy_positive", "new_not_identified", "expected_improvement",
      "Only the legacy global character substitution equates zeta with ceta.",
      list(document_text = "Excipiente sintetico: ceta"),
      list(document_text = "Excipiente sintetico: ceta"),
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "G", "Substring-only occurrence", "lactosa",
      "legacy_positive", "new_not_identified", "expected_improvement",
      "Word-boundary matching rejects lactosado as evidence for lactosa.",
      list(document_text = "Material sintetico lactosado"),
      list(document_text = "Material sintetico lactosado"),
      "complete", "taxonomy"
    ),
    new_parallel_validation_case(
      "H", "Regex metacharacters remain literal", "A+B? (tipo 1) [x].*",
      "legacy_negative", "new_identified", "expected_improvement",
      "Observed legacy grepl interprets metacharacters; the new matcher treats the query literally.",
      list(document_text = "A+B? (tipo 1) [x].*"),
      list(document_text = "A+B? (tipo 1) [x].*"),
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "I", "Case and diacritic-only differences", "LÁCTOSA",
      "legacy_positive", "new_identified", "equivalent",
      "Both normalization strategies identify case and diacritic variants.",
      list(document_text = "Contiene lactosa"),
      list(document_text = "Contiene lactosa"),
      "complete", "taxonomy"
    ),
    new_parallel_validation_case(
      "J", "Literal fallback present", "Crospovidona",
      "legacy_positive", "new_identified", "equivalent",
      "A safe literal query preserves public search capability outside the taxonomy.",
      list(document_text = "Crospovidona tipo A"),
      list(document_text = "Crospovidona tipo A"),
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "K", "Literal fallback absent", "Crospovidona",
      "legacy_negative", "new_not_identified", "equivalent",
      "Both engines report absence from valid complete content; provenance remains literal.",
      list(document_text = "Excipientes: sacarosa"),
      list(document_text = "Excipientes: sacarosa"),
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "L", "Zero medicines", "lactosa",
      "legacy_error", "new_no_products", "expected_improvement",
      "The new service returns a valid empty search result instead of the legacy sequence defect.",
      list(zero_medicines = TRUE),
      list(zero_products = TRUE),
      new_expected_result_count = 0L
    ),
    new_parallel_validation_case(
      "M", "Multiblock section 6.1 with product-level presence", "Excipiente X",
      "legacy_positive", "new_identified", "equivalent",
      "This case validates product-level presence only.",
      multiblock,
      multiblock,
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "N", "Multiblock section 6.1 with product-level absence", "Excipiente W",
      "legacy_negative", "new_not_identified", "equivalent",
      "Complete multiblock content supports absence only at MedicinalProduct level.",
      multiblock,
      multiblock,
      "complete", "literal"
    ),
    new_parallel_validation_case(
      "O", "Structured positive conflicts with valid section 6.1 no-match", "lactosa",
      "legacy_negative", "new_conflicting", "expected_improvement",
      "The new engine preserves official-source conflict instead of flattening it.",
      list(document_text = "Excipientes: sacarosa"),
      list(structured_entries = list("lactosa"), document_text = "Excipientes: sacarosa"),
      "complete", "taxonomy"
    ),
    new_parallel_validation_case(
      "P", "Unsupported HTML content", "lactosa",
      "legacy_positive", "new_indeterminate", "expected_improvement",
      "The legacy strips HTML; the new factual policy refuses unsupported representation as exhaustive.",
      list(document_text = "<p>Contiene lactosa</p>"),
      list(document_text = "<p>Contiene lactosa</p>", document_mode = "unsupported"),
      "failed", "taxonomy"
    ),
    new_parallel_validation_case(
      "Q", "No section 6.1 source and no structured positive", "lactosa",
      "legacy_negative", "new_indeterminate", "expected_improvement",
      "Source absence is not excipient absence.",
      list(document_status = 404L),
      list(document_mode = "absent"),
      "failed", "taxonomy"
    ),
    new_parallel_validation_case(
      "R", "Multiple formulations with one structured positive", "lactosa",
      "legacy_negative", "new_identified", "expected_improvement",
      "Positive evidence from any formulation aggregates to product-level presence.",
      list(document_status = 503L),
      list(structured_entries = list("lactosa", "sacarosa"), document_mode = "absent"),
      "partial", "taxonomy"
    ),
    new_parallel_validation_case(
      "S", "Ambiguous product documents", "lactosa",
      "legacy_negative", "new_indeterminate", "expected_improvement",
      "The new service does not choose one of two equally eligible documents arbitrarily.",
      list(document_status = 503L),
      list(document_mode = "ambiguous"),
      "failed", "taxonomy"
    ),
    new_parallel_validation_case(
      "T", "Ambiguous taxonomy alias", "alias compartido",
      "legacy_positive", "new_query_ambiguous", "expected_improvement",
      "Taxonomic ambiguity stops before sources rather than silently choosing a concept.",
      list(document_text = "Contiene alias compartido"),
      list(taxonomy_mode = "ambiguous", document_text = "Contiene alias compartido"),
      new_expected_result_count = 0L,
      new_expected_source_calls = 0L
    )
  )
}

