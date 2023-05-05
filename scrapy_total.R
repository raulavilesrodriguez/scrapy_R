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
# order for Date of creation
file_info <- file.info(pages)
file_info <- file_info[with(file_info, order(as.POSIXct(mtime))), ]
pages_ordered <- rownames(file_info)

# List of Projects in Development
development_titulos <- list()
development_links <- list()

# List of Properties Finished
properties_titulos <- list()
properties_links <- list()

pages_ordered <- list(pages_ordered)

# ....SCRAPY OF EACH PAGE....
# Total characteristics of PROPERTY FINISHIED
list_total <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  caracteristicas <- data |> html_elements(xpath = '//div/div[@data-qa="posting PROPERTY"]') |> html_text2()
  print(caracteristicas)
  properties_titulos <- c(
    properties_titulos,
    caracteristicas
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
  unlist(list_total),
  unlist(list_links_properties)
)
names(df_properties) <- c('total', 'links')


# Total characteristics of PROPERTY IN DEVELOPMENT
list_development <- sapply(pages_ordered[[1]], function(page){
  print(page)
  data <-read_html(page, encoding="UTF-8")
  caracteristicas <- data |> html_elements(xpath = '//div/div[@data-qa="posting DEVELOPMENT"]') |> html_text2()
  print(caracteristicas)
  development_titulos <- c(
    development_titulos,
    caracteristicas
  )
})
# links of PROPERTY IN DEVELOPMENT
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
  unlist(list_development),
  unlist(list_links_development)
)
names(df_development) <- c('total','link')

#____________Wrangling____________



