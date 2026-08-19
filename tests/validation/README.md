# Parallel validation

This directory declares deterministic comparison scenarios for the protected
legacy behavior and the new application service. It is a validation layer, not
another search engine.

Each case records separate legacy, new-engine, and safety expectations. A
deliberate semantic difference is green when its closed verdict is
`expected_improvement`; differences are not flattened into ordinary pass/fail
equivalence. `requires_review` is reserved for genuinely unresolved findings.

The test runner supplies a strict in-memory HTTP mock to legacy and fake source
ports to the new service. Any unexpected legacy request fails immediately. No
real adapter or external network is used.

Cases M and N concern only `MedicinalProduct` conclusions. They do not establish
composition for an individual formulation or presentation.

Run the matrix with the normal suite:

```r
testthat::test_dir("tests/testthat")
```
