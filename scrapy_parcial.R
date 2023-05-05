library(tidyverse)
library(rvest)
library(httr)

#______SCRAPY WEB_______
# Algoritm to download all pages
set.seed(1)
page_id <- 0

while (TRUE) {
  result <- try({
    page_id <- page_id + 1
    url_web <- paste0('https://www.plusvalia.com/inmuebles-en-venta-en-quito-pagina-',page_id,'.html')
    dest_page <- paste0('data/page-',page_id,'.html')
    download.file(url_web, destfile = dest_page, quiet=TRUE)
    if (page_id == 1550) stop('error')  #1550 is the last web page
    url_web
  }, silent = TRUE)
  if (inherits(result, 'try-error')){
    message('breaking. It is the end')
    break
  } else message(sprintf('The page is %s', result))
}

# list of files in a Directory
pages <- (list.files(path = 'data/', pattern = '.html', all.files = TRUE, full.names = TRUE, recursive=FALSE))
# order for Date
file_info <- file.info(pages)
file_info <- file_info[with(file_info, order(as.POSIXct(mtime))), ]
pages_ordered <- rownames(file_info)

# List of Projects in Development
development_titulos <- list()
development_precios <- list()
development_area <- list()
development_links <- list()

# List of Properties Finished
properties_titulos <- list()
properties_precios <- list()
properties_area <- list()
properties_links <- list()

pages_ordered <- list(pages_ordered)

# ....SCRAPY OF EACH PAGE....
# Location of PROPERTY FINISHIED
list_location_properties <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  print(data |> html_elements(xpath = '//div/div[@class="sc-ge2uzh-1 gFoERJ"]'))
  titulos_property <- data |> html_elements(xpath = '//div/div[@class="sc-ge2uzh-1 gFoERJ"]')
  properties_titulos <- c(
    properties_titulos,
    titulos_property |> html_text2()
  )
})
# price of PROPERTY FINISHIED
list_price_properties <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  price <- data |> html_elements(xpath = '//div/div[@data-qa="POSTING_CARD_PRICE"]')
  print(price)
  properties_precios <- c(
    properties_precios,
    price |> html_text2()
  )
})
# features AREA of PROPERTY FINISHIED
list_area_properties <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  features_2 <- data |> html_elements(xpath = '//div/div[@class="sc-i1odl-5 eodGhu"]')
  area_2 <- features_2 |> html_text2()
  print(area_2)
  properties_area <- c(
    properties_area,
    area_2
  )
})
# links of PROPERTY FINISHIED
list_links_properties <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  links_2 <- data |> html_elements(xpath = '//div/div[@data-qa="posting PROPERTY"]') |> html_attr('data-to-posting')
  print(links_2)
  properties_links <- c(
    properties_links,
    links_2
  )
})

# converting the list containg the scraped data into tibble
df_properties <- tibble(
  unlist(list_location_properties),
  unlist(list_price_properties),
  #unlist(list_area_properties),
  unlist(list_links_properties)
)

names(df_properties) <- c('ubicación', 'precio', 'area', 'links')

# Location of DEVELOPMENT
list_location_development <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  titulos_development <- data |> html_elements(xpath ='//div/div[@class="sc-i1odl-17 kiOodf"]')
  print(titulos_development |> html_text2())
  development_titulos <- c(
    development_titulos,
    titulos_development |> html_text2()
  )
})
# price of DEVELOPMENT
list_price_development <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  price_from <- data |> html_elements(xpath = '//div/div[@data-qa="POSTING_CARD_PRICE_FROM"]')
  print(price_from)
  development_precios <- c(
    development_precios,
    price_from |> html_text2()
  )
})
# features AREA of DEVELOPMENT
list_area_development <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  features_1 <- data |> html_elements(xpath = '//div/ul[@data-qa="POSTING_CARD_FEATURES"]')
  area_1 <- features_1 |> html_element('li:nth-child(3)') |> html_text2()
  print(area_1)
  development_area <- c(
    development_area,
    area_1
  )
})
# links of DEVELOPMENT
list_links_development <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  links_1 <- data |> html_elements(xpath = '//div/div[@data-qa="posting DEVELOPMENT"]') |> html_attr('data-to-posting')
  print(links_1)
  development_links <- c(
    development_links,
    links_1
  )
})

# converting the list containg the scraped data into tibble
df_development <- tibble(
  unlist(list_location_development),
  #unlist(list_price_development),
  unlist(list_area_development),
  unlist(list_links_development)
)
names(df_development) <- c('ubicación', 'area', 'link') 




