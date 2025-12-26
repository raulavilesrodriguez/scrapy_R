library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(here)

file_old <- "filter/results/nuevas_propiedades_09dic2025.xlsx"
file_new <- "filter/results/nuevas_propiedades_25dic2025.xlsx"

df_old <- read_xlsx(file_old)
df_new <- read_xlsx(file_new)

df_old <- df_old |>
  select(
    id_plusvalia,
    valor_old = valor
  ) |>
  filter(!is.na(id_plusvalia))

df_new <- df_new |>
  mutate(valor_new =valor) |>
  filter(!is.na(id_plusvalia))

df_compare <- df_new |>
  inner_join(df_old, by = "id_plusvalia") |>
  filter(valor_new < valor_old) |>
  mutate(
    diferencia = abs(valor_new - valor_old),
    porcentaje_bajada = round((diferencia / valor_old) * 100, 2)
  ) |>
  arrange(desc(porcentaje_bajada))

# Export
date_old <- gsub("nuevas_propiedades_|\\.xlsx", "", basename(file_old))
date_new <- gsub("nuevas_propiedades_|\\.xlsx", "", basename(file_new))
nombre_salida <- str_c("bajaron_precio_", date_old, "_a_", date_new, ".xlsx")

write_xlsx(df_compare, here::here("filter/results/bajaron_precio", nombre_salida))
message("Archivo exportado: ", nombre_salida)

