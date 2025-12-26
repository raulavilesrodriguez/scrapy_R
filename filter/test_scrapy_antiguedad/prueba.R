library(tidyverse)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr)
library(chromote)
library(magick)
library(tesseract)

page <- "filter/depart.html"

data <-read_html(page, encoding="UTF-8")

all_text <- data |> as.character()

antiquity <- str_extract(all_text, "Publicado hace[^'\"<]+")
print(antiquity)


#-----PRUEBA LINK REAL-----
test_url <- "https://www.plusvalia.com/propiedades/clasificado/veclcain-la-prensa-hermoso-edificio-rentero-en-venta-741-147918208.html"

resp <- httr::GET(test_url,
                  httr::add_headers("User-Agent"="Mozilla/5.0"))

txt <- as.character(content(resp, "text", encoding="UTF-8"))

grepl("Publicado", txt)

scrape_antiquity_scroll_full <- function(url) {
  library(chromote)
  library(magick)
  library(tesseract)
  library(base64enc)
  library(stringr)
  
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
  text <- ocr(img, engine = tesseract("spa"))
  
  cat("=== TEXTO OCR ===\n")
  print(text)
  
  str_extract(text, "(Publicado|P) hace[^\n]+")
}

scrape_antiquity_scroll_full(test_url)







