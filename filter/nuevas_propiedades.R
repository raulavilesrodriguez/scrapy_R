library(readxl)
library(dplyr)
library(stringr)
library(writexl)

normalize_link <- function(link) {
  link <- as.character(link)
  link <- str_trim(link)                      # quitar espacios
  link <- tolower(link)                       # pasar a minúsculas
  link <- sub("\\?.*$", "", link)             # quitar query string (todo desde ?)
  link <- gsub("utm_[a-zA-Z0-9_]+=[^&]*", "", link) # eliminar parámetros utm si quedan
  link <- gsub("(&)+", "&", link)             # limpiar & redundantes (por si acaso)
  link <- sub("/+$", "", link)                # quitar barras finales
  link <- sub("^https?://", "", link)         # quitar esquema http(s) para comparar solo host+path
  link <- sub("^www\\.", "", link)            # quitar www.
  link[link == ""] <- NA                      # convertir cadenas vacías a NA
  return(link)
}

extraer_id_plusvalia <- function(link) {
  as.numeric(str_extract(link, "(?<=-)[0-9]+(?=\\.html)"))
}

# 🔥 en lugar de scraping
nuevas_propiedades <- function(file_actual, exportar = TRUE) {
  
  # Leer archivo
  df <- read_xlsx(file_actual)
  
  # Verificar que exista la columna links
  if (!("links" %in% names(df))) {
    stop("Bro el archivo debe contener la columna 'links' que identifica cada propiedad.")
  }
  
  # Convertir la columna links en texto (por si algún día viene con factor o número raro)
  df <- df |> mutate(
    id_plusvalia = extraer_id_plusvalia(links)
    ) |> 
    filter(!is.na(id_plusvalia)) |>
    distinct(id_plusvalia, .keep_all = TRUE) |>  # eliminar duplicados en actual
    arrange(desc(id_plusvalia)) # más nuevos arriba
    
  # Exportar a Excel si se desea
  if (exportar) {
    date_curr <- gsub("properties_procesado_|\\.xlsx", "", basename(file_actual))
    nombre_salida <- str_c("nuevas_propiedades_", date_curr, ".xlsx")
    
    write_xlsx(df, here::here("filter/results", nombre_salida))
    message("Archivo exportado: ", nombre_salida)
  }
  
  df
}
