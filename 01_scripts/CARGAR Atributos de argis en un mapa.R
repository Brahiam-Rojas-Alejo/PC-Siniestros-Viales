# =========================================================
# Proyecto: Politicas Publicas
# Tema: Siniestralidad vial en Bogotá – FALLECIDOS (muertos)
# Dataset: Fallecidos en siniestros viales
# Fuente: Secretaría Distrital de Movilidad (SIMUR) - FeatureServer ArcGIS
# Autor: [tu nombre]
# Fecha: [hoy]
# =========================================================
#
# ÍNDICE DE OBJETOS PRINCIPALES (dónde se definen y qué filtro llevan)
# -------------------------------------------------------------------
# muertos   Data frame. Definido: sección 7. Filtro: WHERE_MUERTOS (sección 4.1)
#           = año de ocurrencia 2016–2025. Base de TODOS los fallecidos.
# moto      Data frame. Definido: sección 10. Origen: muertos. Filtro:
#           CONDICION_A == "MOTOCICLISTA". Base de fallecidos en moto.
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
ruta_tablas_word <- file.path(ruta_docs, "tablas_para_word")

# Carpeta para tablas en formato listo para copiar a Word (CSV abribles en Excel)
if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE)

# Al copiar una tabla de la consola a Word: seleccione el texto pegado >
# Insertar > Tabla > Convertir texto en tabla > Separar en: Tabulaciones.

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
# Este filtro se usa al descargar la capa de muertos (sección 7).
# Quien define el período es la variable WHERE_MUERTOS (cláusula SQL "where").

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
# 7. Descargar dataset: MUERTOS — AQUÍ SE DEFINE LA BASE PRINCIPAL
# =========================================================
#
# OBJETO:   muertos (data frame / tibble)
# DEFINIDO: En esta sección (líneas siguientes).
# FILTRO:   where = WHERE_MUERTOS (definido en sección 4.1):
#           "ANO_OCURRENCIA_ACC >= 2016 AND ANO_OCURRENCIA_ACC <= 2025"
#           → Solo registros de fallecidos con año de ocurrencia entre 2016 y 2025.
# ORIGEN:   Capa ArcGIS id_muertos (sección 5), descargada con download_layer().
#
# A partir de aquí, "muertos" es la base de TODOS los muertos del período.
# Las secciones que usan "muertos": 8, 8.1, 9, 9.1, 9.2, 11 (serie total).
# =========================================================

muertos <- download_layer(id_muertos,
                          "muertos_bogota_2016_2025",
                          where = WHERE_MUERTOS)

cat("\n--- Total en el período ---\n")
cat("Cantidad total de muertos (2016–2025):", nrow(muertos), "\n")
cat("(Columnas en la base:", ncol(muertos), ")\n")
cat("---\n\n")




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
# BASE PRINCIPAL LISTA PARA ANÁLISIS
# =========================================================
# Objeto usado:  muertos (data frame)
# Definido en:   Sección 7 (descarga con filtro WHERE_MUERTOS).
# Filtro:        Temporal 2016–2025 (año de ocurrencia). Sin filtro por condición
#                (peatón, moto, etc.); incluye todos los fallecidos del período.
# =========================================================

# =========================================================
# 8.1 Cobertura temporal de la base (muertos en general)
# =========================================================
# OBJETO: muertos (definido sección 7). Sin filtro adicional; se usa la base completa.
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
    slice(1:6) %>%
    mutate(mes = format(fecha_mes, "%Y-%m")) %>%
    select(mes, n)
  cat("\nÚltimos meses con datos (muertos en general):\n")
  write.table(as.data.frame(ultimos_meses), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
  if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
  readr::write_csv(ultimos_meses, file.path(ruta_tablas_word, "cobertura_ultimos_meses_muertos.csv"))
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
# 9.1 Vista general: muertos por año y proporción por CONDICION_A
# =========================================================
# OBJETO: muertos (definido sección 7; filtro WHERE_MUERTOS, 2016–2025). Sin filtro adicional.
# Gráfico para ver el total anual y la composición (motociclista, peatón, etc.) antes del análisis por subgrupos.

library(ggplot2)

muertos_anio_condicion <- muertos %>%
  mutate(CONDICION_A = ifelse(is.na(CONDICION_A), "No informada", as.character(CONDICION_A))) %>%
  count(ANO_OCURRENCIA_ACC, CONDICION_A, name = "muertes") %>%
  mutate(CONDICION_A = reorder(factor(CONDICION_A), -muertes, sum)) %>%
  group_by(ANO_OCURRENCIA_ACC) %>%
  mutate(
    total_anio = sum(muertes),
    pct = 100 * muertes / total_anio,
    etiqueta = paste0(muertes, " (", round(pct, 1), "%)")
  ) %>%
  ungroup()

ggplot(muertos_anio_condicion, aes(x = factor(ANO_OCURRENCIA_ACC), y = muertes, fill = CONDICION_A)) +
  geom_col(position = "stack") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 2.8, color = "white", fontface = "bold") +
  labs(
    title = "Muertos por año y condición (CONDICION_A)",
    subtitle = "Bogotá, 2016–2025. En cada segmento: cantidad y % del año.",
    x = "Año", y = "Número de muertes", fill = "Condición"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# Resumen en consola: proporción por CONDICION_A (todos los años)
prop_condicion <- muertos %>%
  mutate(CONDICION_A = ifelse(is.na(CONDICION_A), "No informada", as.character(CONDICION_A))) %>%
  count(CONDICION_A, name = "muertes") %>%
  mutate(prop = round(100 * muertes / sum(muertes), 1)) %>%
  arrange(desc(muertes)) %>%
  rename(Condicion = CONDICION_A, Muertes = muertes, Porcentaje = prop)
cat("\n--- Proporción de muertos por CONDICION_A (total período) ---\n")
write.table(as.data.frame(prop_condicion), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
readr::write_csv(prop_condicion, file.path(ruta_tablas_word, "proporcion_muertos_por_condicion.csv"))
cat("---\n\n")

# =========================================================
# 9.2 Muertos por sexo desagregado por CONDICION_A (general)
# =========================================================
# OBJETO: muertos (definido sección 7; filtro WHERE_MUERTOS, 2016–2025). Sin filtro adicional.

if ("GENERO" %in% names(muertos)) {
  muertos_sexo_condicion <- muertos %>%
    mutate(
      sexo = ifelse(is.na(GENERO) | as.character(GENERO) == "", "No informado", as.character(GENERO)),
      CONDICION_A = ifelse(is.na(CONDICION_A), "No informada", as.character(CONDICION_A))
    ) %>%
    count(sexo, CONDICION_A, name = "muertes") %>%
    mutate(CONDICION_A = reorder(factor(CONDICION_A), -muertes, sum))

  ggplot(muertos_sexo_condicion, aes(x = sexo, y = muertes, fill = CONDICION_A)) +
    geom_col(position = "stack") +
    geom_text(
      aes(label = muertes),
      position = position_stack(vjust = 0.5),
      size = 2.8,
      color = "white",
      fontface = "bold"
    ) +
    labs(
      title = "Muertos por sexo desagregado por condición (CONDICION_A)",
      subtitle = "Bogotá, 2016–2025. Todos los fallecidos.",
      x = "Sexo", y = "Número de muertes", fill = "Condición"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0),
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
} else {
  cat("\n(No hay columna GENERO en muertos; se omite el gráfico por sexo y condición.)\n")
}

# =========================================================
# 10. Submuestra: Motociclistas fallecidos — AQUÍ SE DEFINE "moto"
# =========================================================
#
# OBJETO:   moto (data frame / tibble)
# DEFINIDO: En esta sección (líneas siguientes).
# ORIGEN:   muertos (definido en sección 7; base de todos los muertos 2016–2025).
# FILTRO:   CONDICION_A == "MOTOCICLISTA"
#           → Solo filas donde la condición de la persona es motociclista.
#           No se usa filtro por MUERTE_POSTERIOR (interesan todos los fallecidos en moto).
#
# A partir de aquí, las secciones de MOTOCICLISTAS usan el objeto "moto".
# Secciones que usan "moto": 11 (panel moto), 12, 13, 14, 15, 16, 17, 18, 19, 20.
# =========================================================

library(dplyr)
library(ggplot2)

moto <- muertos %>%
  filter(CONDICION_A == "MOTOCICLISTA")

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
  ultimos_meses_moto <- ultimos_meses_moto %>% select(mes, n) %>% rename(Muertes = n)
  cat("\n--- Últimos meses con datos: MOTOCICLISTAS fallecidos ---\n")
  write.table(as.data.frame(ultimos_meses_moto), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
  if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
  readr::write_csv(ultimos_meses_moto, file.path(ruta_tablas_word, "cobertura_ultimos_meses_motociclistas.csv"))
  cat("---\n\n")
}

# =========================================================
# 11. Muertos por año: total y motociclistas (un solo panel)
# =========================================================
# Un solo panel: ambas series en la misma escala. Con todos los motociclistas
# (sin filtro MUERTE_POSTERIOR) la serie moto ronda ~200–280 por año y es legible.

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
  summarise(total = sum(muertes), .groups = "drop") %>%
  rename(Condicion_luz = luz_solar, Muertes = total)
cat("\n--- Muertes de motociclistas según luz solar ---\n")
write.table(as.data.frame(resumen_luz_moto), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
readr::write_csv(resumen_luz_moto, file.path(ruta_tablas_word, "motociclistas_segun_luz_solar.csv"))
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
# 16. Localidades: tabla, top 10 y todas (barras horizontales)
# =========================================================
# OBJETO: moto (definido sección 10; filtro CONDICION_A == "MOTOCICLISTA" sobre muertos).
# Tabla y gráficos: muertes de motociclistas por localidad.

top_loc <- moto %>%
  count(LOCALIDAD, name = "muertes") %>%
  arrange(desc(muertes)) %>%
  slice(1:10) %>%
  rename(Localidad = LOCALIDAD, Muertes = muertes)

cat("\n--- Top 10 localidades: muertes de motociclistas ---\n")
write.table(as.data.frame(top_loc), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
readr::write_csv(top_loc, file.path(ruta_tablas_word, "top10_localidades_motociclistas.csv"))
cat("---\n\n")

# Gráfico: Top 10 localidades, barras horizontales (mayor a menor: el de más muertes arriba)
top_loc_graf <- top_loc %>% mutate(Localidad = reorder(Localidad, Muertes))
ggplot(top_loc_graf, aes(x = Localidad, y = Muertes)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(aes(label = Muertes), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Top 10 localidades: muertes de motociclistas",
    subtitle = "Bogotá, 2016–2025. Ordenado de mayor a menor.",
    x = "", y = "Número de muertes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

# Gráfico: Todas las localidades (barras horizontales, mayor a menor)
loc_todas <- moto %>%
  count(LOCALIDAD, name = "muertes") %>%
  arrange(desc(muertes)) %>%
  mutate(LOCALIDAD = reorder(factor(LOCALIDAD), muertes))
ggplot(loc_todas, aes(x = LOCALIDAD, y = muertes)) +
  geom_col(fill = "steelblue", width = 0.75) +
  coord_flip() +
  labs(
    title = "Todas las localidades: muertes de motociclistas",
    subtitle = "Bogotá, 2016–2025. Ordenado de mayor a menor.",
    x = "", y = "Número de muertes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = rel(0.85)),
    panel.grid.major.y = element_blank()
  )


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

moto_edad_export <- moto_edad %>% rename(Grupo_edad = lustro, Muertes = muertes)
cat("\n--- Muertes de motociclistas por grupo de edad (lustros) ---\n")
write.table(as.data.frame(moto_edad_export), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
readr::write_csv(moto_edad_export, file.path(ruta_tablas_word, "motociclistas_por_edad_lustros.csv"))
cat("---\n\n")

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


# =========================================================
# 18. Muertes de motociclistas por género
# =========================================================

if ("GENERO" %in% names(moto)) {
  moto_genero <- moto %>%
    mutate(genero = ifelse(is.na(GENERO) | as.character(GENERO) == "", "No informado", as.character(GENERO))) %>%
    count(genero, name = "muertes") %>%
    arrange(desc(muertes))

  cat("\n--- Muertes de motociclistas por género ---\n")
  write.table(as.data.frame(moto_genero), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
  if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
  readr::write_csv(moto_genero %>% rename(Genero = genero, Muertes = muertes), file.path(ruta_tablas_word, "motociclistas_por_genero.csv"))
  cat("---\n\n")

  ggplot(moto_genero, aes(x = reorder(genero, -muertes), y = muertes)) +
    geom_col(fill = "steelblue", width = 0.6) +
    geom_text(aes(label = muertes), vjust = -0.3, size = 4) +
    labs(
      title = "Muertes de motociclistas por género",
      subtitle = "Bogotá, 2016–2025",
      x = "Género", y = "Número de muertes"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0),
      plot.title = element_text(face = "bold")
    )
} else {
  cat("\n(No hay columna GENERO en los datos de motociclistas; se omite el gráfico por género.)\n")
}


# =========================================================
# 19. CLASE_ACC en motociclistas fallecidos (conteo y proporción)
# =========================================================

if ("CLASE_ACC" %in% names(moto)) {
  moto_clase <- moto %>%
    mutate(clase = ifelse(is.na(CLASE_ACC) | as.character(CLASE_ACC) == "", "No informado", as.character(CLASE_ACC))) %>%
    count(clase, name = "muertes") %>%
    mutate(prop = round(100 * muertes / sum(muertes), 1)) %>%
    arrange(desc(muertes)) %>%
    mutate(clase = reorder(factor(clase), -muertes))

  cat("\n--- Motociclistas fallecidos por CLASE_ACC (conteo y %) ---\n")
  write.table(as.data.frame(moto_clase), file = "", sep = "\t", row.names = FALSE, quote = FALSE)
  if (!exists("ruta_tablas_word")) { ruta_tablas_word <- file.path(getwd(), "03_documentos", "tablas_para_word"); if (!dir.exists(ruta_tablas_word)) dir.create(ruta_tablas_word, recursive = TRUE) }
  readr::write_csv(moto_clase %>% rename(Clase_accidente = clase, Muertes = muertes, Porcentaje = prop), file.path(ruta_tablas_word, "motociclistas_por_clase_accidente.csv"))
  cat("---\n\n")

  ggplot(moto_clase, aes(x = clase, y = muertes)) +
    geom_col(fill = "steelblue", width = 0.65) +
    geom_text(aes(label = paste0(muertes, " (", prop, "%)")), vjust = -0.2, size = 3.2) +
    labs(
      title = "Motociclistas fallecidos por clase de accidente (CLASE_ACC)",
      subtitle = "Bogotá, 2016–2025. En cada barra: cantidad y % del total.",
      x = "Clase de accidente", y = "Número de muertes"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
} else {
  cat("\n(No hay columna CLASE_ACC en los datos; se omite el gráfico por clase de accidente.)\n")
}


# =========================================================
# 20. Interacciones (motociclistas): género × lustro y localidad × año
# =========================================================

# 20.1 Género × Grupo de edad (lustros)
if ("GENERO" %in% names(moto)) {
  moto_edad_gen <- moto %>%
    mutate(
      edad_num = as.numeric(EDAD),
      genero = ifelse(is.na(GENERO) | as.character(GENERO) == "", "No informado", as.character(GENERO))
    ) %>%
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
    count(lustro, genero, name = "muertes") %>%
    mutate(lustro = factor(lustro, levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
                                              "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                                              "70-74", "75-79", "80-84", "85+")))
  ggplot(moto_edad_gen, aes(x = lustro, y = muertes, fill = genero)) +
    geom_col(position = "dodge") +
    labs(
      title = "Interacción: género × grupo de edad (motociclistas)",
      subtitle = "Bogotá, 2016–2025",
      x = "Grupo de edad (lustros)", y = "Muertes", fill = "Género"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# 20.2 Localidades × Año (top 5 localidades)
top5_loc <- moto %>%
  count(LOCALIDAD, name = "n") %>%
  slice_max(n, n = 5) %>%
  pull(LOCALIDAD)
moto_loc_anio <- moto %>%
  filter(LOCALIDAD %in% top5_loc) %>%
  count(LOCALIDAD, ANO_OCURRENCIA_ACC, name = "muertes")
ggplot(moto_loc_anio, aes(x = ANO_OCURRENCIA_ACC, y = muertes, color = LOCALIDAD)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title = "Interacción: localidad × año (top 5 localidades, motociclistas)",
    subtitle = "Bogotá, 2016–2025",
    x = "Año", y = "Muertes", color = "Localidad"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "top")