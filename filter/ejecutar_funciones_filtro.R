library(here)

source(here("filter/nuevas_propiedades.R"))
source(here("filter/bajaron_precio.R"))
source(here("filter/propiedades_vendidas.R"))
source(here("filter/filtrado_zonas_precio.R"))
source(here("filter/input_filter.R"))

# Old Propiedades
df_anterior_propiedades <- nuevas_propiedades(file_anterior, exportar = FALSE)

# Nuevas Propiedades
df_nuevas_propiedades <- nuevas_propiedades(file_new, exportar = FALSE)

df_nuevas_propiedades_filt <- df_nuevas_propiedades |>
  slice_head(n = n_nuevas)

# Bajaron Precio
df_bajaron_precio <- bajaron_precio(
  df_anterior_propiedades, 
  df_nuevas_propiedades, 
  exportar = FALSE
  )


#-------- FILTRADO ZONAS Y PRECIO --------#
# Filtrado de todo el archivo
filtrado_zonas_precio(
  df_nuevas_propiedades,
  zonas_objetivo, 
  precio_maximo, 
  num_habitaciones, 
  num_bathrooms,
  zonas_excluir,
  "filtrado_zonas_precio_",
  debug = TRUE
)

# Filtrado de solo nuevas propiedades
filtrado_zonas_precio(
  df_nuevas_propiedades_filt, 
  zonas_objetivo, 
  precio_maximo, 
  num_habitaciones, 
  num_bathrooms,
  zonas_excluir,
  "filtrado_solo_nuevas_"
)

# Filtrado de solo las que bajaron de precio
filtrado_zonas_precio(
  df_bajaron_precio, 
  zonas_objetivo, 
  precio_maximo, 
  num_habitaciones, 
  num_bathrooms,
  zonas_excluir,
  "filtrado_bajaron_precio_"
)
