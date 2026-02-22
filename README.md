# Siniestralidad vial en Bogotá

Proyecto de políticas públicas: análisis de fallecidos y lesionados en siniestros viales en Bogotá (2016–2025).

**Fuente:** Secretaría Distrital de Movilidad (SIMUR) – [FeatureServer ArcGIS](https://sig.simur.gov.co/arcgis/rest/services/Accidentalidad/AccidentalidadAnalisis/FeatureServer).

## Estructura

- `01_scripts/` – Scripts R para descargar datos y análisis (muertos, lesionados, motociclistas).
- `02_datos/` – Datos descargados (CSV, RDS).
- `03_documentos/` – Documentos del proyecto.
- `04_notas/` – Notas.

## Uso

1. En R, establecer el directorio de trabajo en la raíz del proyecto (`setwd(".../PC")`).
2. Ejecutar los scripts en `01_scripts/`:
   - **CARGAR Atributos de argis en un mapa.R** – Fallecidos (muertos) y análisis de motociclistas.
   - **CARGAR Lesionados ArcGIS en un mapa.R** – Lesionados.

Dependencias: `httr`, `jsonlite`, `dplyr`, `purrr`, `lubridate`, `readr`, `ggplot2`.
