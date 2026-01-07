library(here)

source(here("filter/nuevas_propiedades.R"))
source(here("filter/bajaron_precio.R"))
source(here("filter/propiedades_vendidas.R"))
source(here("filter/filtrado_zonas_precio.R"))

file <- "results/properties_procesado_28dic2025.xlsx"

nuevas_propiedades(file)


file_old <- "filter/results/nuevas_propiedades_16dic2025.xlsx"
file_new <- "filter/results/nuevas_propiedades_28dic2025.xlsx"
bajaron_precio(file_old, file_new)

precio_maximo <- 100000
num_habitaciones <- 2
num_bathrooms <- 2

zonas_objetivo <- c(
  "la carolina",
  "monteserrin",
  "republica del salvador",
  "quito tenis",
  "jardines de amagasi",
  "tumbaco",
  "cumbaya",
  "miravalle",
  "la primavera",
  "bellavista",
  "gonzalez suarez",
  "gaspar de villarroel",
  "quicentro"
)

filtrado_zonas_precio(
  file_new, 
  zonas_objetivo, 
  precio_maximo, 
  num_habitaciones, 
  num_bathrooms
  )









