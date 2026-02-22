# =========================================================
# Proyecto: Politicas Publicas
# Tema: Siniestralidad vial en Bogotá – FALLECIDOS (muertos)
# Dataset: Fallecidos en siniestros viales
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
# URL base del servicio (capa 0 = MUERTO)
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

# Míralas en consola o en View(capas)
capas

# =========================================================
# 4.1 Filtro temporal para esta entrega (2016–2025)
# =========================================================

WHERE_MUERTOS <- "ANO_OCURRENCIA_ACC >= 2016 AND ANO_OCURRENCIA_ACC <= 2025"

# =========================================================
# 5. Identificar capa de MUERTOS
# =========================================================
# Si no encuentra, asigna manualmente mirando `capas` (ej. id_muertos <- 0).

id_muertos <- capas |>
  mutate(name_low = tolower(name)) |>
  filter(grepl("muert|fallec", name_low)) |>
  slice(1) |>
  pull(id)

# Revisa qué ID encontró:
id_muertos


# =========================================================
# 6. Funciones para descargar datos desde ArcGIS (paginado)
# =========================================================
# Por qué necesitamos esto:
# - ArcGIS impone límites (ej. 2000 registros por request).
# - Por eso, descargamos en "páginas" con resultOffset/resultRecordCount.

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
  
  # URL específica de esa capa
  layer_url <- paste0(fs_url, "/", layer_id)
  
  # Contar registros
  total <- arcgis_count(layer_url, where = where)
  message("Descargando: ", nombre_salida, " | Registros: ", total)
  
  # Crear offsets para paginar:
  # usamos total-1 para evitar un offset final que cae en vacío
  offsets <- seq(0, max(0, total - 1), by = page_size)
  
  # Barra de progreso (para no sentir que se congeló)
  pb <- txtProgressBar(min = 0, max = length(offsets), style = 3)
  i <- 0
  
  # Descargar página por página y unir todo (filtrar páginas vacías/NULL)
  paginas <- purrr::map(offsets, function(off) {
    i <<- i + 1
    setTxtProgressBar(pb, i)
    arcgis_page(layer_url, where = where, offset = off, page_size = page_size)
  })
  paginas <- purrr::compact(paginas)
  datos <- if (length(paginas) > 0L) dplyr::bind_rows(paginas) else dplyr::tibble()
  
  close(pb)
  
  # Guardar en tu carpeta 02_datos
  saveRDS(datos, file.path(ruta_datos, paste0(nombre_salida, ".rds")))
  readr::write_csv(datos, file.path(ruta_datos, paste0(nombre_salida, ".csv")))
  
  return(datos)
}


# =========================================================
# 7. Descargar dataset: MUERTOS
# =========================================================

muertos <- download_layer(id_muertos,
                          "muertos_bogota_2016_2025",
                          where = WHERE_MUERTOS)

cat("\nFilas muertos:", nrow(muertos),
    " | Columnas:", ncol(muertos), "\n")




# =========================================================
# 8. (Opcional) Conversión de fechas (para análisis)
# =========================================================
# En tus datos, campos como FECHA_OCURRENCIA_ACC suelen venir como:
# - numérico grande (milisegundos desde 1970-01-01) => "Unix epoch ms"
# Para convertirlos: as.POSIXct(fecha_ms/1000, origin="1970-01-01", tz="America/Bogota")

# Ejemplo (solo si la columna existe):
if ("FECHA_OCURRENCIA_ACC" %in% names(muertos)) {
  muertos <- muertos |>
    mutate(
      fecha_ocurrencia = as.POSIXct(FECHA_OCURRENCIA_ACC / 1000,
                                    origin = "1970-01-01",
                                    tz = "America/Bogota"),
      anio = year(fecha_ocurrencia),
      mes  = month(fecha_ocurrencia),
      fecha_mes = as.Date(floor_date(fecha_ocurrencia, unit = "month"))
    )
}

# Guardar versión "con fecha" si quieres
saveRDS(muertos, file.path(ruta_datos, "muertos_bogota_con_fecha.rds"))

# =========================================================
# 8.1 Cobertura temporal de la base (muertos en general)
# =========================================================
# Para saber hasta qué fecha llegan realmente los datos en la fuente.

if ("fecha_ocurrencia" %in% names(muertos)) {
  rango_muertos <- range(muertos$fecha_ocurrencia, na.rm = TRUE)
  cat("\n--- Cobertura temporal: MUERTOS (base descargada) ---\n")
  cat("Fecha mínima:", format(rango_muertos[1], "%Y-%m-%d"), "\n")
  cat("Fecha máxima:", format(rango_muertos[2], "%Y-%m-%d"), "\n")
  # Últimos meses con datos (cuántos registros por mes)
  ultimos_meses <- muertos %>%
    filter(!is.na(fecha_mes)) %>%
    count(fecha_mes, name = "n") %>%
    arrange(desc(fecha_mes)) %>%
    slice(1:6)
  cat("\nÚltimos meses con datos (muertos en general):\n")
  print(ultimos_meses)
  cat("---\n\n")
} else if ("FECHA_OCURRENCIA_ACC" %in% names(muertos)) {
  fechas_num <- muertos$FECHA_OCURRENCIA_ACC
  fechas_num <- fechas_num[!is.na(fechas_num) & fechas_num > 0]
  if (length(fechas_num) > 0) {
    rango_muertos <- as.POSIXct(range(fechas_num, na.rm = TRUE) / 1000, origin = "1970-01-01", tz = "America/Bogota")
    cat("\n--- Cobertura temporal: MUERTOS (base descargada) ---\n")
    cat("Fecha mínima:", format(rango_muertos[1], "%Y-%m-%d"), "\n")
    cat("Fecha máxima:", format(rango_muertos[2], "%Y-%m-%d"), "\n")
    cat("---\n\n")
  }
}

# =========================================================
# 9. (Opcional) Serie mensual de muertos
# =========================================================
if ("fecha_mes" %in% names(muertos)) {
  serie_muertos <- muertos |>
    count(fecha_mes, name = "muertos") |>
    arrange(fecha_mes)
  write_csv(serie_muertos, file.path(ruta_datos, "serie_mensual_muertos.csv"))
  head(serie_muertos, 12)
}

# =========================================================
# 10. Submuestra: Motociclistas fallecidos (2016–2025)
# =========================================================

library(dplyr)
library(ggplot2)

moto <- muertos %>%
  filter(
    MUERTE_POSTERIOR == "S",
    CONDICION_A == "MOTOCICLISTA"
  )

cat("\nMotociclistas fallecidos:", nrow(moto), "\n")

# Cobertura temporal de motociclistas (comparar con muertos en general)
# (fecha_mes = primer día del mes solo para agrupar; el "01" es la etiqueta del mes, no el día del accidente)
if ("fecha_mes" %in% names(moto)) {
  ultimos_meses_moto <- moto %>%
    filter(!is.na(fecha_mes)) %>%
    count(fecha_mes, name = "n") %>%
    arrange(desc(fecha_mes)) %>%
    slice(1:6) %>%
    mutate(mes = format(fecha_mes, "%Y-%m"))
  cat("\n--- Últimos meses con datos: MOTOCICLISTAS fallecidos ---\n")
  print(ultimos_meses_moto %>% select(mes, n))
  cat("---\n\n")
}

# =========================================================
# 11. Muertos por año: total y motociclistas (superpuestos)
# =========================================================

muertos_anio <- muertos %>%
  count(ANO_OCURRENCIA_ACC, name = "muertes") %>%
  mutate(tipo = "Total")
moto_anio <- moto %>%
  count(ANO_OCURRENCIA_ACC, name = "muertes") %>%
  mutate(tipo = "Motociclistas")
serie_anual_ambas <- bind_rows(muertos_anio, moto_anio)

ggplot(serie_anual_ambas, aes(x = ANO_OCURRENCIA_ACC, y = muertes, color = tipo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_color_manual(values = c("Total" = "gray40", "Motociclistas" = "steelblue")) +
  labs(
    title = "Muertes por año: total y motociclistas (Bogotá, 2016–2025)",
    x = "Año", y = "Número de muertes", color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# =========================================================
# 12. Muertes por hora (solo motociclistas) y luz solar
# =========================================================
# Clasificación aproximada para Bogotá: 6:00–17:59 = con luz solar; 18:00–5:59 = sin luz solar.

moto_hora_luz <- moto %>%
  mutate(
    hora = as.integer(substr(HORA_OCURRENCIA_ACC, 1, 2)),
    luz_solar = ifelse(hora >= 6 & hora <= 17, "Con luz solar", "Sin luz solar")
  ) %>%
  filter(!is.na(hora), hora >= 0, hora <= 23) %>%
  count(hora, luz_solar, name = "muertes")

# Resumen: total muertes moto con vs sin luz solar
resumen_luz_moto <- moto_hora_luz %>%
  group_by(luz_solar) %>%
  summarise(total = sum(muertes), .groups = "drop")
cat("\n--- Muertes de motociclistas según luz solar ---\n")
print(resumen_luz_moto)
cat("---\n\n")

ggplot(moto_hora_luz, aes(x = hora, y = muertes, fill = luz_solar)) +
  geom_col(position = "stack") +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_manual(values = c("Con luz solar" = "#F4A261", "Sin luz solar" = "#2A9D8F")) +
  labs(
    title = "Muertes de motociclistas por hora y condición de luz (Bogotá, 2016–2025)",
    subtitle = "Con luz solar: 6:00–17:59 | Sin luz solar: 18:00–5:59",
    x = "Hora del día (0–23)", y = "Número de muertes", fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# =========================================================
# 13. Figura 2 – Tendencia anual solo motociclistas
# =========================================================

serie_anual <- moto %>%
  count(ANO_OCURRENCIA_ACC, name = "muertes")

ggplot(serie_anual, aes(x = ANO_OCURRENCIA_ACC, y = muertes)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title = "Muertes de motociclistas (Bogotá, 2016–2025)",
    x = "Año", y = "Muertes"
  )

# =========================================================
# 14. Figura 3 – Distribución horaria (motociclistas)
# =========================================================

moto_h <- moto %>%
  mutate(
    hora = as.integer(substr(HORA_OCURRENCIA_ACC, 1, 2)),
    finde = ifelse(DIA_OCURRENCIA_ACC %in% c("SABADO","DOMINGO"),
                   "Fin de semana", "Entre semana")
  ) %>%
  filter(!is.na(hora), hora >= 0, hora <= 23) %>%
  count(hora, finde)

ggplot(moto_h, aes(x = hora, y = n, fill = finde)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Distribución horaria de muertes motociclistas",
    x = "Hora (0–23)", y = "Muertes", fill = ""
  )

# =========================================================
# 15. Figura 4 – Puntos calientes (Hotspots)
# =========================================================

moto_xy <- moto %>% filter(!is.na(x), !is.na(y))

ggplot(moto_xy, aes(x = x, y = y)) +
  stat_bin2d(bins = 90) +
  coord_equal() +
  labs(
    title = "Hotspots de mortalidad motociclista (2016–2025)",
    x = "X", y = "Y"
  )

# =========================================================
# 16. Top 10 localidades
# =========================================================

top_loc <- moto %>%
  count(LOCALIDAD, name = "muertes") %>%
  arrange(desc(muertes)) %>%
  slice(1:10)

top_loc


# =========================================================
# 17. Análisis por edades: motociclistas fallecidos (lustros)
# =========================================================
# Grupos etarios de 5 años (lustros); edades no válidas se excluyen.

moto_edad <- moto %>%
  mutate(edad_num = as.numeric(EDAD)) %>%
  filter(!is.na(edad_num), edad_num >= 0, edad_num <= 120) %>%
  mutate(
    lustro = cut(
      edad_num,
      breaks = c(seq(0, 85, by = 5), Inf),
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                 "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                 "70-74", "75-79", "80-84", "85+"),
      include.lowest = TRUE,
      right = FALSE
    )
  ) %>%
  count(lustro, name = "muertes") %>%
  mutate(lustro = factor(lustro, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                                            "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                                            "70-74", "75-79", "80-84", "85+")))

cat("\nMotociclistas fallecidos con edad válida:", sum(moto_edad$muertes), "\n")

ggplot(moto_edad, aes(x = lustro, y = muertes)) +
  geom_col(fill = "steelblue", width = 0.75) +
  geom_text(aes(label = muertes), vjust = -0.3, size = 3) +
  labs(
    title = "Muertes de motociclistas por grupo de edad (lustros)",
    subtitle = "Bogotá, 2016–2025",
    x = "Grupo de edad (años)",
    y = "Número de muertes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )