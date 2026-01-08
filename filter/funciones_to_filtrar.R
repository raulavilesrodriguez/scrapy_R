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

detectar_zona <- function(texto, zonas_objetivo, zonas_excluir) {
  if(any(str_detect(texto, str_c("\\b", zonas_excluir, "\\b")))) {
    return(NA_character_)
  } 
  zona_encontrada <- zonas_objetivo[str_detect(texto, str_c("\\b", zonas_objetivo, "\\b"))]
  if (length(zona_encontrada) == 0) NA_character_ else zona_encontrada[1]
}

zonas_no_deseadas <- function(texto, zonas_excluir) {
  any(str_detect(texto, str_c("\\b", zonas_excluir, "\\b")))
}