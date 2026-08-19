# ExciFinder

ExciFinder es una aplicación Shiny para recuperar, organizar y analizar de forma determinista información oficial sobre excipientes publicada en CIMA por la Agencia Española de Medicamentos y Productos Sanitarios (AEMPS).

Su objetivo es facilitar la revisión de fuentes oficiales para medicamentos autorizados y comercializados. La evaluación actual se realiza a nivel de `MedicinalProduct` (número de registro), no de cada `Presentation` comercial.

## Interpretación de resultados

Cada medicamento recibe un estado factual y una cobertura de verificación independientes.

Estados:

- **Identificado**: existe evidencia del excipiente en una fuente analizada.
- **No identificado en fuentes verificadas**: no se encontró evidencia en las fuentes que la política factual considera suficientes y que pudieron analizarse completamente.
- **No verificable**: la información disponible o la ejecución técnica no permiten concluir.
- **Fuentes discordantes**: las fuentes analizadas producen evidencia incompatible.

La cobertura indica si la verificación fue completa, parcial, fallida o no realizada. Un estado y su cobertura deben interpretarse conjuntamente; por ejemplo, un resultado identificado puede tener cobertura parcial.

## Búsqueda y fuentes

El matching es determinista y usa términos controlados cuando existe una resolución inequívoca. Una taxonomía inicial mínima permite además una búsqueda literal normalizada con validación estricta de entrada. No se usan coincidencias fuzzy, modelos generativos ni LLM.

El motor consulta:

- la sección 6.1 de la ficha técnica segmentada disponible en CIMA;
- datos estructurados de excipientes como fuente complementaria;
- metadatos oficiales del medicamento y enlaces a la ficha técnica.

La evidencia original, la sección y la procedencia se conservan para auditoría y exportación.

## Limitaciones conocidas

- La evaluación es a nivel del medicamento autorizado y no determina la composición específica de cada presentación comercial.
- Los datos estructurados de excipientes no se consideran exhaustivos para establecer ausencia.
- La sección 6.1 solo se trata como fuente exhaustiva a nivel de producto cuando se recupera y analiza completamente bajo las condiciones actuales de la política factual.
- Los errores técnicos producen resultados no verificables o cobertura incompleta; nunca se convierten en resultados negativos.
- El motor nuevo todavía no implementa fallback de extracción desde PDF.
- “No identificado” no constituye certeza absoluta de ausencia.
- La ficha técnica oficial debe verificarse antes de tomar decisiones clínicas.

ExciFinder es una herramienta de recuperación, organización y análisis determinista de información oficial y no sustituye la valoración de profesionales sanitarios.

## Instalación reproducible

El proyecto declara R 4.5.2 y sus paquetes en `renv.lock`.

```r
install.packages("renv")
renv::restore()
```

`pdftools` y su dependencia de sistema se conservan temporalmente únicamente para las pruebas de regresión legacy.

## Ejecución local

Desde la raíz del repositorio:

```r
shiny::runApp(".")
```

El punto de entrada canónico es `app.R`. Su carga construye clientes y servicios, pero no realiza solicitudes a CIMA hasta que el usuario inicia una búsqueda.

## Arquitectura

```text
Shiny UI/server
      ↓
ExcipientSearchService
      ↓
domain + factual policy
      ↓
ports
      ↓
adapters CIMA
```

`R/domain/` contiene el modelo factual; `R/application/` los casos de uso, matching y ports; `R/adapters/` la integración CIMA; y `R/ui/` la presentación Shiny. El motor anterior permanece aislado en `tests/legacy/` únicamente para characterization tests y la matriz comparativa A–T.

## Tests y CI

La suite es hermética y no usa CIMA real:

```r
testthat::test_dir("tests/testthat")
renv::status()
```

GitHub Actions ejecuta la suite, comprueba `renv` y valida que `app.R` produce un objeto `shiny.appobj` en cada push o pull request dirigido a `main`.

## Fuente y atribución

Los datos y documentos proceden de CIMA, servicio de la AEMPS. ExciFinder no implica aval institucional de la AEMPS y debe utilizarse junto con la ficha técnica oficial.

Para comunicar resultados incorrectos, fallos reproducibles o problemas de documentación, abra un GitHub Issue en este repositorio evitando incluir datos personales o clínicos identificables.

## Licencia

Este repositorio todavía no declara una licencia. La elección corresponde al propietario y debe resolverse antes de un release público.
