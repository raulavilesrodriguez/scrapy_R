library(tidyverse)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr) # to split strings
library(chromote)
library(magick)
library(tesseract)
library(base64enc)
library(writexl)
library(here)


# OCR español listo una sola vez (rápido)
tess_engine <- tesseract("spa")

extraer_nuevos <- function(file){
  
  db <- read_excel(file)
  
  scrape_antiquity_scroll_full <- function(url) {
    
    tryCatch({
      b <- ChromoteSession$new()
      
      b$Network$setUserAgentOverride(
        userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Chrome/122 Safari/537.36"
      )
      
      b$Page$navigate(url)
      Sys.sleep(10)  # cloudflare
      
      # obtener altura total
      get_height <- function() {
        h <- b$Runtime$evaluate("document.body.scrollHeight")$result$value
        return(as.numeric(h))
      }
      
      total_height <- get_height()
      viewport <- b$Runtime$evaluate("window.innerHeight")$result$value
      
      y <- 0
      while (y < total_height - viewport) {
        js <- sprintf("window.scrollTo(0, %d);", y)
        b$Runtime$evaluate(js)
        Sys.sleep(1)
        
        # actualizar altura (algunos sitios la cambian dinámicamente)
        total_height <- get_height()
        
        y <- y + viewport
      }
      
      # screenshot final, ya al fondo
      shot <- b$Page$captureScreenshot(format = "png")
      b$close()
      
      img <- image_read(base64decode(shot$data))
      text <- ocr(img, engine = tess_engine)
      
      # Limpieza OCR
      text <- str_squish(text)
      
      #cat("=== TEXTO OCR ===\n")
      #print(text)
      
      # Extraer antigüedad info
      str_extract(text, "(Publicado|P) hace[^\n]+")
      
      if (is.na(antig)) antig <- "NO ENCONTRADO"
      return(antig)
      
    }, error = function(e){
      message("Error en la URL: ", url)
      return("ERROR EN LA EXTRACCIÓN")
    })
  }
  
  # scraping antiquity for each link
  db <- db |>
    mutate(antiguaedad = map_chr(links, ~{
      message("Procesando URL: ", .x)
      scrape_antiquity_scroll_full(.x)
    }))
  
  
  nombre_salida <- str_c(basename(file), "_", "antiguaedad.xlsx")
  
  write_xlsx(db, here::here("filter", nombre_salida))
  message("Archivo exportado: ", nombre_salida)
  
  return(db)
}

