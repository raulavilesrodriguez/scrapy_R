library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)

normalizar_texto <- function(x) {
  x %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>% 
    str_replace_all("[^a-z ]", " ") %>%
    str_squish()
}

# eliminar outliers usando IQR
quitar_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  x[x >= (q1 - 1.5 * iqr) & x <= (q3 + 1.5 * iqr)]
}

filtrado_zonas_precio <- function(file, zonas_objetivo, precio_maximo, num_habitaciones, num_bathrooms) {
  # Leer archivo
  df <- read_xlsx(file)
  
  # Normalizar zonas objetivo
  df <- df |>
    mutate(
      ubicacion_norm = normalizar_texto(ubicacion_general)
    )
  
  df <- df |>
    mutate(
      zona = map_chr(ubicacion_norm, function(texto){
        zona_encontrada <- zonas_objetivo[str_detect(texto, zonas_objetivo)]
        if (length(zona_encontrada) == 0) {
          NA_character_
        } else {
          zona_encontrada[1]
        }
      })
    )
  
  df_filtrado <- df |> 
    filter(
      !is.na(zona),
      valor <= precio_maximo,
      valor > 20000
    )
  
  precio_m2_zona <- df_filtrado |>
    group_by(zona) |>
    summarise(
      precio_m2_promedio = mean(
        quitar_outliers_iqr(valor_metro), 
        na.rm = TRUE
        ),
      num_propiedades = n(),
      .groups = "drop"
    ) |>
    filter(!is.na(zona))
  
  # Oportunidades
  oportunidades <- df_filtrado |>
    left_join(precio_m2_zona, by = "zona") |>
    filter(
      valor_metro < precio_m2_promedio,
      habitaciones >= num_habitaciones,
      baños >= num_bathrooms
      ) |>
    arrange(valor_metro)
  
  date_file <- gsub("nuevas_propiedades_|\\.xlsx", "", basename(file))
  nombre_salida <- str_c("filtrado_zonas_precio_", date_file, ".xlsx")
  write_xlsx(oportunidades, here::here("filter/results/filtrado_zonas_precio", nombre_salida))
  message("Archivo exportado: ", nombre_salida)
  
  oportunidades
  
}
