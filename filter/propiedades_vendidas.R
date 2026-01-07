library(readxl) 
library(dplyr) 
library(stringr) 
library(writexl)

propiedades_vendidas <- function(file_previo, file_actual, exportar = TRUE) { 
  # Leer archivos 
  df_prev <- read_xlsx(file_previo) 
  df_curr <- read_xlsx(file_actual) 
   
  # ----------------------------- 
  # Comparar y obtener propiedades vendidas 
  # ----------------------------- 
  vendidas <- df_prev |>
    anti_join(df_curr, by = c("id_plusvalia")) |>
    arrange(desc(id_plusvalia))
  
  # Exportar a Excel si se desea 
  if (exportar) { 
    date_prev <- gsub("properties_procesado_|\\.xlsx", "", basename(file_previo)) 
    date_curr <- gsub("properties_procesado_|\\.xlsx", "", basename(file_actual)) 
    nombre_salida <- str_c("propiedades_vendidas", date_prev, "_", date_curr, ".xlsx") 
    write_xlsx(vendidas, here::here("filter/results/propiedades_vendidas", nombre_salida)) 
    message("Archivo exportado: ", nombre_salida) 
  }
  
  vendidas 
}