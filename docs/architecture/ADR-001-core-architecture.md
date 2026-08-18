# ADR-001: Core architecture for ExciFinder

## Status

Accepted.

## Context

ExciFinder is currently a R/Shiny application with a legacy CIMA search engine.
The engine and its known behaviour are protected by characterization tests. The
application needs to grow from the current active-ingredient/excipient lookup
to traceable searches, alternatives, comparisons, filters, and future local
indexing without coupling the core to either Shiny or CIMA.

## Decision

Adopt a **modular R/Shiny monolith with an independent canonical domain,
application services, and lightweight ports/adapters**.

- Shiny is an interface, not the engine.
- CIMA is a data-source adapter, not the domain.
- The domain does not depend on HTTP, JSON, PDF, Excel, Shiny, or storage.
- The factual engine is separate from any future clinical interpretation.
- Migration is incremental; the legacy engine remains temporarily available.

The intended directory shape is advisory until migration requires it:

```text
R/
├── domain/
├── application/
├── adapters/
├── ui/
└── legacy/
```

`app.R` is the composition root and Shiny interface. Application services use
domain contracts and ports. Adapters implement ports and may depend on CIMA,
HTTP, JSON, PDF, or storage. Domain code must not depend on adapters or UI.

## Domain model

| Concept | Purpose and identity |
|---|---|
| `MedicinalProduct` | Authorized medicinal product; initial identity is `authority + nregistro`. |
| `Formulation` | The composition-relevant level between `MedicinalProduct` and `Presentation`; one formulation can serve multiple presentations. Its definitive identity is intentionally deferred pending CIMA data analysis. |
| `Presentation` | Concrete commercial presentation; preferred identity is `authority + codigo_nacional`. It is not interchangeable with `Formulation`. |
| `Excipient` | Canonical concept independent of CIMA with a stable internal ID, canonical name, controlled synonyms, linguistic variants, E codes, future families/relations, and regulatory references. |
| `SourceArtifact` | Official versionable, traceable provenance unit: either a `structured_record` or a `document`. |
| `ExcipientEvidence` | Concrete identification evidence: excipient, matching term, `source_artifact_id`, section when applicable, excerpt, method, and location when available. |
| `VerificationAttempt` | Value object recording `source`, `source_artifact_id`, `method`, `section`, `retrieved_at`, `outcome`, `extraction_status`, `error`, and nested `evidence`. |
| `ExcipientAssessment` | Factual conclusion for a formulation or product and an excipient; references the subject, coverage, attempts, technical errors, matcher version, and taxonomy version. Evidence remains nested in attempts. |
| `SearchResult` | Application DTO, not a central persistent entity. |

`SourceArtifact` distinguishes provenance without requiring every source to be a
document:

```text
SourceArtifact
├── structured_record
└── document
```

## Verification semantics

Factual conclusion is independent from verification quality:

```text
factual_conclusion: identified | not_identified | indeterminate | conflicting
verification_coverage: complete | partial | failed | not_attempted
technical_errors, verification_attempts, and evidence: separate fields
```

`unverifiable` is not a domain factual conclusion. A UI may later use it as a
presentation label, for example for `indeterminate + failed` coverage.
`not_identified` requires sufficient coverage under the applicable retrieval
policy; lack of evidence alone is not sufficient.

## Matching and sources

The factual matching pipeline is:

```text
raw text → safe normalization → canonical excipient resolution
         → deterministic matching → ExcipientEvidence
```

Initial matching favours Unicode-aware controlled normalization, literal search,
term boundaries, versioned synonyms, and explicit auditable rules. It excludes
global phonetic substitutions, embeddings, LLMs, and probabilistic matching.

Retrieval policy belongs to application/adapters, not the domain. Its initial
priority may be: structured official information, official section 6.1, other
official representations, then official PDF fallback. Every source attempt
creates a `VerificationAttempt`.

## Factual and clinical boundary

```text
Factual engine → immutable factual assessment → future clinical rules engine
```

Clinical rules are outside v1.x. They may not modify factual evidence and must
be versioned and validated independently.

## Technology and ports

- Use light S3 for entities and value objects.
- Use data.frame/tibble for collections and tabular results.
- Use named lists of functions for ports.
- Use structured R conditions for errors.
- Use R6 only if a concrete future mutable-state requirement justifies it.

`SourceArtifactPort` retrieves artifact provenance and content.
`CompositionSourcePort` exposes source-native composition as
`SourceExcipientEntry` application DTOs. These entries are not canonical
`Excipient` objects and do not perform resolution or matching. Evidence refers
to its provenance through `source_artifact_id`.

Repositories start behind ports and may initially use memory. SQLite, DuckDB,
or another persistent implementation can later replace that adapter without
changing the domain or services.

## Migration strategy

```text
legacy baseline
→ domain contracts
→ ports
→ CIMA adapter / mapper
→ document extraction + attempts
→ taxonomy + deterministic matcher
→ new search service
→ parallel comparison with legacy
→ Shiny switches to new service
→ legacy removal
```

Each stage keeps the existing suite green. The new engine must not translate
`indeterminate` to `FALSE` for compatibility. Legacy code can be removed only
after the new service is adopted and protected by equivalent or stronger tests.

## Deferred decisions

- Definitive `Formulation` identity.
- SQLite, DuckDB, or other persistence.
- Future conversion to an R package.
- A dedicated API.
- Parallelization.
- Incremental update strategy.
- Deeper Shiny modularization.

## Consequences

The project accepts modest mapping and wiring overhead in exchange for
independence from CIMA and Shiny, auditable evidence, and independently
testable factual and future clinical concerns.

Out of scope for now: PostgreSQL, microservices, mandatory Docker, a REST API,
golem, extensive R6, async/parallelization, real OCR, embeddings, LLMs, a
clinical rules engine, authentication, and user management.
