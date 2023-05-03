library(tidyverse)
library(rvest)
library(httr)

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

list_titulos <- list()
list_precios <- list()
list_area <- list()
list_links <- list()

lapply(pages_ordered, function(pagina){
  data <-read_html(pagina, encoding="UTF-8")
  #class(data)
  #html_text(data)
})

data <-read_html('data/page-1.html', encoding="UTF-8")
titulos <- data |> html_elements(xpath ='//div[@data-qa="POSTING_CARD_LOCATION"]')
list_titulos <- c(
  list_titulos,
  titulos |> html_text2()
)

# price of DEVELOPMENT
price_from <- data |> html_elements(xpath = '//div/div[@data-qa="POSTING_CARD_PRICE_FROM"]')
list_precios <- c(
  list_precios,
  price_from |> html_text2()
)
#price of PROPERTY FINISHIED
price <- data |> html_elements(xpath = '//div/div[@data-qa="POSTING_CARD_PRICE"]')
list_precios <- c(
  list_precios,
  price |> html_text2()
)
# features of DEVELOPMENT
features_1 <- data |> html_elements(xpath = '//div/ul[@data-qa="POSTING_CARD_FEATURES"]')
area_1 <- features_1 |> html_element('li:nth-child(3)') |> html_text2()
list_area <- c(
  list_area,
  area_1
)
# features of PROPERTY FINISHIED
features_2 <- data |> html_elements(xpath = '//div/div[@data-qa="POSTING_CARD_FEATURES"]')
area_2 <- features_2 |> html_element('span:nth-child(1)') |> html_text2()
list_area <- c(
  list_area,
  area_2
)
# links of DEVELOPMENT
links_development <- data |> html_elements(xpath = '//div/div[@data-qa="posting DEVELOPMENT"]') |> html_attr('data-to-posting')
list_links <- c(
  list_links,
  links_development
)
# links of PROPERTY FINISHIED
links_property <- data |> html_elements(xpath = '//div/div[@data-qa="posting PROPERTY"]') |> html_attr('data-to-posting')
list_links <- c(
  list_links,
  links_property
)




