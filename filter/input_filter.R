#---INPUTS ---
file_anterior <- "results/properties_procesado_16dic2025.xlsx"
file_new <- "results/properties_procesado_28dic2025.xlsx"
n_nuevas <- 500

precio_maximo <- 100000
num_habitaciones <- 2
num_bathrooms <- 2
porcnt_reventa <- 0.90
retorno_esperado <- 0.15

zonas_objetivo <- c(
  "carolina",
  "monteserrin",
  "campo alegre",
  "republica de el salvador",
  "republica del salvador",
  "portugal",
  "quito tenis",
  "jardines de amagasi",
  "tumbaco",
  "cumbaya",
  "miravalle",
  "primavera",
  "gonzalez suarez",
  "gaspar de villarroel",
  "batan",
  "carmelo",
  "labrador",
  "granda centeno",
  "bellavista"
  #"bosque",
  #"eloy alfaro",
  #"quicentro"
)

zonas_excluir <- c(
  "sur", "carcelen", "carretas", "turubamba", "quitumbe", 
  "cesar villacres", "calderon", "cotocollao", "enrique aymer",
  "oficina", "negocio"
  )











