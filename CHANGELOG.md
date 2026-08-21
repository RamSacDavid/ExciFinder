# Changelog

## Unreleased

## 2.1.1 - 2026-08-21

- Mejorado el contraste del estado «Fuentes discordantes» para cumplir WCAG AA con texto blanco.
- Eliminada la dependencia residual no utilizada de `shinydashboard`.
- Endurecida la comprobación de consistencia de `renv` en integración continua.
- Añadido acceso visible a la aplicación pública y estado de CI en la documentación principal.

## 2.1.0 - 2026-08-21

- Sustituido el shell basado en AdminLTE/shinydashboard por una interfaz Shiny propia, clara, responsive y orientada a búsqueda.
- Reorganizada la búsqueda de principio activo y excipiente como interacción principal y trasladada la exportación Excel a una acción secundaria.
- Sustituida la tabla visible de resultados por una navegación clínica master–detail agrupada por estado factual.
- Añadida una ficha por medicamento con dosis, forma farmacéutica, vía de administración, excipiente consultado, cobertura y fuentes verificadas.
- Añadida visualización individual de la verificación por fuente y de todas las evidencias disponibles, preservando excerpts y términos coincidentes.
- Mejorada la experiencia móvil y tablet sin añadir nuevas dependencias de frontend.
- Mejorada la accesibilidad mediante navegación por teclado, estados ARIA y preservación del foco al cambiar de medicamento.
- Conservados sin cambios la política factual, el matching, el autocomplete, la memoización y el contrato de exportación Excel.

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
