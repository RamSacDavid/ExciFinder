# Changelog

## Unreleased

Target release: 2.0.0

- Nueva arquitectura separada en domain, application, adapters y UI.
- Provenance autocontenida para fuentes y evidencias de cada resultado.
- Estados factuales seguros con cobertura de verificación independiente.
- Matching determinista mediante términos controlados.
- Fallback literal normalizado y seguro para términos no catalogados.
- Nueva UI Shiny basada exclusivamente en `ExcipientSearchService`.
- Eliminación del falso negativo causado por errores técnicos o fuentes incompletas.
- Paginación completa del catálogo de medicamentos relevante.
- Entorno reproducible con `renv` y validación automatizada en CI.
