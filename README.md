# ExciFinder v.1.0

**ExciFinder** es una herramienta avanzada de búsqueda y filtrado de excipientes en medicamentos autorizados por la Agencia Española de Medicamentos y Productos Sanitarios (AEMPS). 

El sistema utiliza un motor híbrido que consulta la API REST de CIMA y, en caso de no encontrar datos estructurados, realiza un análisis de texto mediante la lectura de los documentos PDF oficiales (Fichas Técnicas).

## Características principales

* **Búsqueda Semántica:** Normalización de texto para evitar fallos por tildes o variantes ortográficas (ej. bencilico vs benzílico).
* **Motor Híbrido:** Consulta de secciones segmentadas (JSON) y escaneo de PDF completo en tiempo real.
* **Caché de Sesión:** Optimización de velocidad al evitar re-descargar documentos ya analizados durante la misma consulta.
* **Exportación:** Generación de reportes detallados en formato Excel (.xlsx).
* **Enlaces Directos:** Acceso inmediato a la Ficha Técnica oficial en el portal de CIMA para verificación humana.

## Requisitos e Instalación

Para ejecutar este proyecto en un entorno local de R o RStudio, asegúrese de tener instaladas las siguientes librerías:

```r
install.packages(c("shiny", "shinydashboard", "httr", "jsonlite", 
                   "dplyr", "tidyr", "DT", "stringi", "pdftools", "openxlsx"))
```

## Agradecimientos

Este proyecto ha sido posible gracias a la base de datos pública de la **Agencia Española de Medicamentos y Productos Sanitarios (AEMPS)**. Se agradece especialmente el acceso a la API de **CIMA** (Centro de Información online de Medicamentos de la AEMPS), cuya transparencia y servicios web permiten el desarrollo de herramientas tecnológicas orientadas a la mejora de la seguridad farmacológica y el acceso a la información sanitaria.

## Aviso Legal

La información facilitada por **ExciFinder** se obtiene de fuentes oficiales en tiempo real; no obstante, el desarrollador no se hace responsable de posibles errores en la base de datos de origen o de interpretaciones erróneas de los datos extraídos.

* **No es consejo médico:** Esta aplicación es una herramienta de consulta técnica y soporte a la decisión, pero no sustituye en ningún caso el criterio clínico de un facultativo o profesional sanitario cualificado.
* **Verificación obligatoria:** Se recomienda encarecidamente contrastar cualquier resultado con la **Ficha Técnica (PDF)** oficial enlazada en la propia aplicación antes de tomar cualquier decisión sobre la prescripción o administración de fármacos.
