# Changelog

## Unreleased

## 2.0.1 - 2026-08-20

- Restaurada la presentación separada de resultados según estado factual.
- Añadida codificación cromática por estado: rojo, verde, gris y naranja.
- Restaurado el texto predictivo de principios activos mediante CIMA.
- Añadidas sugerencias contextuales de excipientes a partir de composición estructurada disponible.
- Conservada la búsqueda libre de principios activos y excipientes.
- Añadidas forma farmacéutica y dosis/strength a resultados y exportación.
- Mejorado el layout de tablas con paneles a ancho completo y ocultación de grupos vacíos.
- Reducidas las llamadas repetidas de detalle de medicamento mediante memoización de alcance corto.

## 2.0.0 - 2026-08-19

- Nueva arquitectura separada en domain, application, adapters y UI.
- Provenance autocontenida para fuentes y evidencias de cada resultado.
- Estados factuales seguros con cobertura de verificación independiente.
- Matching determinista mediante términos controlados.
- Fallback literal normalizado y seguro para términos no catalogados.
- Nueva UI Shiny basada exclusivamente en `ExcipientSearchService`.
- Eliminación del falso negativo causado por errores técnicos o fuentes incompletas.
- Paginación completa del catálogo de medicamentos relevante.
- Entorno reproducible con `renv` y validación automatizada en CI.
