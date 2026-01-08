library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(here)

source(here::here("filter/funciones_to_filtrar.R"))


filtrado_zonas_precio <- function(
    df, 
    zonas_objetivo, 
    precio_maximo, 
    num_habitaciones, 
    num_bathrooms, 
    zonas_excluir,
    output_name_file = "",
    exportar = TRUE,
    debug = FALSE
    ) {
  
  zonas_objetivo <- normalizar_texto(zonas_objetivo)
  
  # Normalizar zonas objetivo
  df <- df |>
    mutate(
      ubicacion_norm = normalizar_texto(ubicacion_general),
      links_norm = normalizar_texto(links)
    )
  
  df <- df |>
    mutate(
      zona_ubicacion = map_chr(ubicacion_norm, ~ detectar_zona(.x, zonas_objetivo, zonas_excluir)),
      zona_links = map_chr(links_norm, ~ detectar_zona(.x, zonas_objetivo, zonas_excluir)),
      zona = coalesce(zona_ubicacion, zona_links)
    )
  
  # para depuración: ver casos donde zona_ubicacion es NA pero zona_links no lo es
  if (debug) {
    df_debug <- df |>
      filter(is.na(zona_ubicacion) & !is.na(zona_links)) |>
      count(zona_links)
    
    assign("debug_zonas_link", df_debug, envir = .GlobalEnv)
  }
  
  
  df_filtrado <- df |> 
    filter(
      !is.na(zona),
      habitaciones > 0, # para filtrar solo propiedades no terrenos
      baños > 0 
    )
  
  df_filtrado <- df_filtrado |>
    filter(!map_lgl(ubicacion_norm, ~ zonas_no_deseadas(.x, zonas_excluir))) |> #devuelve true si contiene zona no deseada
    filter(!map_lgl(links_norm, ~ zonas_no_deseadas(.x, zonas_excluir))) #devuelve true si contiene zona no deseada
  
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
    mutate(
      descuento_pct = ((valor_metro/precio_m2_promedio) - 1) * 100
    ) |>
    filter(
      valor <= precio_maximo,
      valor > 20000,
      valor_metro < precio_m2_promedio,
      habitaciones >= num_habitaciones,
      baños >= num_bathrooms
      ) |>
    select(-c(ubicacion_norm, links_norm, zona_ubicacion, zona_links)) |>
    arrange(valor_metro)
  
  oportunidades <- oportunidades |>
    mutate(
      precio_reventa_estimado = precio_m2_promedio * area * 0.95,
      ganancia_bruta = precio_reventa_estimado - valor,
      retorno_pct = ganancia_bruta / valor
    )
  
  
  # Exportar a Excel
  if(exportar){
    date_raw <- stringr::str_extract(
      basename(file),
      "\\d{1,2}[a-z]{3}\\d{4}"
    )
    
    nombre_salida <- str_c(output_name_file, date_raw, ".xlsx")
    write_xlsx(oportunidades, here::here("filter/results/filtrado_zonas_precio", nombre_salida))
    message("Archivo exportado: ", nombre_salida)
  }
  
  oportunidades
  
}
