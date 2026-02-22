# =========================================================
# Proyecto: Politicas Publicas
# Tema: Siniestralidad vial en Bogotá – LESIONADOS
# Dataset: Lesionados en siniestros viales
# Fuente: Secretaría Distrital de Movilidad (SIMUR) - FeatureServer ArcGIS
# Autor: [tu nombre]
# Fecha: [hoy]
# =========================================================


# =========================================================
# 1. Limpiar entorno
# =========================================================
rm(list = ls())
gc()


# =========================================================
# 2. Librerías (instalar solo una vez si hace falta)
# =========================================================
# install.packages(c("httr", "jsonlite", "dplyr", "purrr", "lubridate", "readr"))
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)
library(readr)


# =========================================================
# 3. Rutas del proyecto (TU ESTRUCTURA)
# =========================================================
ruta_proyecto <- getwd()

ruta_scripts <- file.path(ruta_proyecto, "01_scripts")
ruta_datos   <- file.path(ruta_proyecto, "02_datos")
ruta_docs    <- file.path(ruta_proyecto, "03_documentos")
ruta_notas   <- file.path(ruta_proyecto, "04_notas")

# Verificación básica (deberían dar TRUE)
dir.exists(ruta_proyecto)
dir.exists(ruta_datos)


# =========================================================
# 4. Fuente de datos (ArcGIS FeatureServer) + leer capas
# =========================================================
# URL base del servicio (capa 1 = LESIONADO)
fs_url <- "https://sig.simur.gov.co/arcgis/rest/services/Accidentalidad/AccidentalidadAnalisis/FeatureServer"

# Configuración: el servidor puede estar lento, damos más tiempo a cada request
httr::set_config(httr::timeout(120))

# 4.1 Traer metadatos del servicio (en formato JSON "bonito")
meta <- httr::GET(fs_url, query = list(f = "pjson")) |>
  httr::content("text", encoding = "UTF-8") |>
  jsonlite::fromJSON()

# 4.2 Listar capas disponibles (id + nombre)
capas <- dplyr::tibble(
  id   = meta$layers$id,
  name = meta$layers$name
)

capas

# =========================================================
# 4.1 Filtro temporal para esta entrega (2016–2025)
# =========================================================

WHERE_LESIONADOS <- "ANO_OCURRENCIA_ACC >= 2016 AND ANO_OCURRENCIA_ACC <= 2025"

# =========================================================
# 5. Identificar capa de LESIONADOS
# =========================================================
# Si no encuentra, asigna manualmente mirando `capas` (ej. id_lesionados <- 1).

id_lesionados <- capas |>
  mutate(name_low = tolower(name)) |>
  filter(grepl("lesionad", name_low)) |>
  slice(1) |>
  pull(id)

# Revisa qué ID encontró:
id_lesionados


# =========================================================
# 6. Funciones para descargar datos desde ArcGIS (paginado)
# =========================================================
# ArcGIS impone límites (ej. 2000 registros por request).
# Descargamos en "páginas" con resultOffset/resultRecordCount.

# 6.1 Contar cuántos registros hay en una capa
arcgis_count <- function(layer_url, where = "1=1") {
  r <- httr::GET(
    paste0(layer_url, "/query"),
    query = list(
      where = where,
      returnCountOnly = "true",
      f = "json"
    )
  )
  httr::stop_for_status(r)
  jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))$count
}

# 6.2 Descargar UNA página de datos
arcgis_page <- function(layer_url, where = "1=1", outFields = "*",
                        offset = 0, page_size = 2000) {

  r <- httr::GET(
    paste0(layer_url, "/query"),
    query = list(
      where = where,
      outFields = outFields,
      returnGeometry = "true",
      resultOffset = offset,
      resultRecordCount = page_size,
      orderByFields = "OBJECTID ASC",
      resultType = "standard",
      f = "json"
    )
  )

  httr::stop_for_status(r)

  txt <- httr::content(r, "text", encoding = "UTF-8")
  x <- jsonlite::fromJSON(txt, simplifyVector = FALSE)

  if (!is.null(x$error)) stop(paste0("ArcGIS error: ", x$error$message))
  if (is.null(x$features) || length(x$features) == 0) return(NULL)

  # helper seguro: si no hay coord, devuelve NA
  safe_coord <- function(g, coord) {
    if (is.null(g) || is.null(g[[coord]]) || length(g[[coord]]) == 0) return(NA_real_)
    as.numeric(g[[coord]][1])
  }

  # Normalizar atributos: ArcGIS a veces devuelve campos con longitud 0; as.data.frame falla.
  norm_attr <- function(att) {
    if (is.null(att)) return(NULL)
    lapply(att, function(v) {
      if (is.null(v) || length(v) == 0L) NA else v[[1]]
    })
  }

  rows <- lapply(x$features, function(f) {
    if (is.null(f$attributes)) return(NULL)
    att <- norm_attr(f$attributes)
    if (is.null(att) || length(att) == 0L) return(NULL)
    a <- as.data.frame(att, stringsAsFactors = FALSE)
    a$x <- safe_coord(f$geometry, "x")
    a$y <- safe_coord(f$geometry, "y")
    dplyr::as_tibble(a)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  dplyr::bind_rows(rows)
}

# 6.3 Descargar TODA una capa completa y guardarla
download_layer <- function(layer_id, nombre_salida, where = "1=1", page_size = 2000) {

  layer_url <- paste0(fs_url, "/", layer_id)

  total <- arcgis_count(layer_url, where = where)
  message("Descargando: ", nombre_salida, " | Registros: ", total)

  offsets <- seq(0, max(0, total - 1), by = page_size)

  pb <- txtProgressBar(min = 0, max = length(offsets), style = 3)
  i <- 0

  paginas <- purrr::map(offsets, function(off) {
    i <<- i + 1
    setTxtProgressBar(pb, i)
    arcgis_page(layer_url, where = where, offset = off, page_size = page_size)
  })
  paginas <- purrr::compact(paginas)
  datos <- if (length(paginas) > 0L) dplyr::bind_rows(paginas) else dplyr::tibble()

  close(pb)

  saveRDS(datos, file.path(ruta_datos, paste0(nombre_salida, ".rds")))
  readr::write_csv(datos, file.path(ruta_datos, paste0(nombre_salida, ".csv")))

  return(datos)
}


# =========================================================
# 7. Descargar dataset: LESIONADOS
# =========================================================

lesionados <- download_layer(id_lesionados,
                             "lesionados_bogota_2016_2025",
                             where = WHERE_LESIONADOS)

cat("\nFilas lesionados:", nrow(lesionados),
    " | Columnas:", ncol(lesionados), "\n")


# =========================================================
# 8. (Opcional) Conversión de fechas (para análisis)
# =========================================================
if ("FECHA_OCURRENCIA_ACC" %in% names(lesionados)) {
  lesionados <- lesionados |>
    mutate(
      fecha_ocurrencia = as.POSIXct(FECHA_OCURRENCIA_ACC / 1000,
                                    origin = "1970-01-01",
                                    tz = "America/Bogota"),
      anio = year(fecha_ocurrencia),
      mes  = month(fecha_ocurrencia),
      fecha_mes = as.Date(floor_date(fecha_ocurrencia, unit = "month"))
    )
}

# Guardar versión "con fecha"
saveRDS(lesionados, file.path(ruta_datos, "lesionados_bogota_con_fecha.rds"))


# =========================================================
# 9. (Opcional) Serie mensual de lesionados
# =========================================================
if ("fecha_mes" %in% names(lesionados)) {
  serie_lesionados <- lesionados |>
    count(fecha_mes, name = "lesionados") |>
    arrange(fecha_mes)
  write_csv(serie_lesionados, file.path(ruta_datos, "serie_mensual_lesionados.csv"))
  head(serie_lesionados, 12)
}


# =========================================================
# 10. (Opcional) Resumen por localidad
# =========================================================
if ("LOCALIDAD" %in% names(lesionados)) {
  top_loc_lesionados <- lesionados |>
    count(LOCALIDAD, name = "lesionados") |>
    arrange(desc(lesionados)) |>
    slice(1:10)
  top_loc_lesionados
}
