library(readxl)
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(purrr)
library(here)
library(ggplot2)

source(here::here("filter/funciones_to_filtrar.R"))


filtrado_zonas_precio <- function(
    df,
    file = NULL,
    zonas_objetivo, 
    precio_maximo, 
    num_habitaciones, 
    num_bathrooms, 
    zonas_excluir,
    output_name_file = "",
    porcnt_reventa,
    retorno_esperado = 0,
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
    filter(!map_lgl(ubicacion_norm, ~ zonas_no_deseadas(.x, zonas_excluir))) #devuelve true si contiene zona no deseada
  
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
    distinct(zona, .keep_all = TRUE)
  
  # Oportunidades
  oportunidades_base <- df_filtrado |>
    left_join(precio_m2_zona, by = "zona") |>
    mutate(
      descuento_pct = (1 -(valor_metro/precio_m2_promedio))
    )
  
  assign("oportunidades_base", oportunidades_base, envir = .GlobalEnv)
  
  
  oportunidades <- oportunidades_base|>
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
      precio_reventa_estimado = precio_m2_promedio * area * porcnt_reventa,
      ganancia_bruta = precio_reventa_estimado - valor,
      retorno_pct = ganancia_bruta / valor
    )
  
  mediana_descuento_zona <- oportunidades |>
    group_by(zona) |>
    summarise(
      mediana_descuento = median(descuento_pct, na.rm = TRUE),
      .groups = "drop"
    )
  
  oportunidades <- oportunidades |>
    left_join(mediana_descuento_zona, by = "zona")
  
  oportunidades <- oportunidades |>
    mutate(
      retorno_norm = pmin(retorno_pct, 0.5) / 0.5
    )
  
  oportunidades <- oportunidades |>
    mutate(
      score = 
        (descuento_pct * 0.4) +
        ((precio_maximo - valor) / precio_maximo * 0.2) +
        (pmin(habitaciones, 3) / 3 * 0.2) +
        (retorno_norm * 0.2)
    ) |>
    arrange(desc(score))
  
  oportunidades<-  oportunidades|>
    filter(!map_lgl(links, ~ zonas_no_deseadas(.x, zonas_excluir))) #devuelve true si contiene zona no deseada
  
  if(retorno_esperado > 0){
    oportunidades <- oportunidades |>
      filter(retorno_norm >= retorno_esperado)
  }
  
  # Gráfico de caja de descuentos por zona
  plot_descuentos <- ggplot(oportunidades, aes(zona, descuento_pct)) +
    geom_boxplot(
      fill = "#6BB6FF",
      alpha = 0.7,
      outlier.color = "#1F5FAA"
    ) +
    coord_flip() +
    labs(
      title = "Descuento vs mercado por zona",
      y = "Descuento %",
      x = ""
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.background = element_rect(fill = "#EDF5FF", color = NA),
      plot.background  = element_rect(fill = "#EDF5FF", color = NA),
      axis.text.y      = element_text(face = "bold"),
      plot.title       = element_text(face = "bold")
    )
  print(plot_descuentos)
  
  # Exportar a Excel
  if(exportar){
    date_raw <- if(!is.null(file)){
      stringr::str_extract(
        basename(file),
        "\\d{1,2}[a-z]{3}\\d{4}")
    } else {
      format(Sys.Date(), "%d%b%Y")
    }
    
    nombre_salida <- str_c(output_name_file, date_raw, ".xlsx")
    write_xlsx(oportunidades, here::here("filter/results/filtrado_zonas_precio", nombre_salida))
    message("Archivo exportado: ", nombre_salida)
  }
  
  oportunidades
  
}
