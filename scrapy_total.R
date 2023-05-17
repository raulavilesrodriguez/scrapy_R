library(tidyverse)
library(rvest)
library(httr)
library(readxl)
library(stringr)
library(purrr) # to split strings
library(hrbrthemes)
library(viridis) # pallette of colors
library(viridisLite) # pallette of colors

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
#Export tibble
writexl::write_xlsx(df_properties, 'properties.xlsx')
writexl::write_xlsx(df_development, 'development.xlsx')
df_properties <- read_excel('properties.xlsx')
df_development <- read_excel('development.xlsx')

df_properties <- df_properties |> 
  mutate(valor = str_extract(total, regex("[USD|$]\\s*\\d+\\.?\\d+\\.?\\d*")),
         alicuota = str_extract(total, regex("USD\\s*\\d+\\s*(?i)Condominio/Alícuota")),
         area = str_extract(total, ifelse(str_detect(total, regex("\\d+\\s*m²"))==TRUE, 
                                          regex("\\d+\\s*m²"), 
                                          regex("\\d+\\s*m2"))),
         habitaciones = str_extract(total, regex("\\d+\\s*(?i)hab.")),
         baños = str_extract(total, regex("\\d+\\s*(?i)baños")),
         estacionamientos = str_extract(total, regex("\\d+\\s*(?i)estac.")),
         ubicacion_general = str_extract(total, regex(".+,\\s+Quito")))

# string splitting
we <- str_split(df_properties$total, '\n')
# add column ubicación específica
df_properties <- df_properties |>
  mutate(ubicacion_especifica = map_chr(we, 3))
# change of character to numeric several columns
df_properties <- df_properties |>
  mutate(valor = str_extract(valor, regex("\\d+\\.?\\d+\\.?\\d*")),
         alicuota = str_extract(alicuota, regex("\\d+")),
         area = str_extract(area, regex("\\d+\\.?\\d?")),
         habitaciones = str_extract(habitaciones, regex("\\d+")),
         baños = str_extract(baños, regex("\\d+")),
         estacionamientos = str_extract(estacionamientos, regex("\\d+"))) |> 
  mutate(valor = str_replace_all(valor, "\\.", ""),
         links = paste('https://www.plusvalia.com', links, sep = ""))

df_properties[,c(3:8)] <- lapply(df_properties[,c(3:8)], as.numeric)
df_properties[c(3:8)][is.na(df_properties[c(3:8)])] <- 0

# add new columns
df_properties <- df_properties |> mutate(valor_metro = ifelse(area >0, round(valor / area, digits = 2), 0))

# delete duplicates
df_properties <- df_properties |> distinct()

# export df properties final
writexl::write_xlsx(df_properties, 'properties_procesado.xlsx')

#_____Analisis of Data______
# Exploration of data of Properties tibble
label1 <- df_properties |> group_by(habitaciones) |> summarise(num_propiedades = n()) |>
  mutate(proporcion = num_propiedades / nrow(df_properties))

grafico_1 <- df_properties |> group_by(habitaciones) |> summarise(num_propiedades = n()) |>
  mutate(proporcion = num_propiedades / nrow(df_properties)) |>
  ggplot(aes(habitaciones, num_propiedades, size = proporcion)) +
  geom_point(alpha = 0.6, aes(color= proporcion)) +
  scale_size(range = c(.1, 20), name="Propiedades x habitaciones") +
  theme(legend.position="bottom") +
  ylab("No. Propiedades") +
  xlab("Habitaciones") +
  geom_text(
    label=label1$habitaciones, #to insert labels to the graph
    nudge_x=0, nudge_y=0.1,
    size = 3,
    check_overlap=T
  )
grafico_1 +
  scale_color_viridis(option = "D") +
  scale_fill_viridis()

grafico_2 <- df_properties |> ggplot(aes(habitaciones, log10(area), color= valor_metro)) +
  scale_color_gradient(low="green",high="darkgreen")+
  geom_point()
grafico_2

grafico_3 <- df_properties |> select(valor) |> ggplot(aes(log10(valor))) +
  geom_density(fill="#99627A") +
  labs(x = "Valor (normalizado)")
grafico_3

df_filtered <- df_properties |> filter(valor >0 & valor < 218000000)
grafico_4 <- df_filtered |> ggplot(aes(c(1: nrow(df_filtered)), valor, color=valor)) + 
  scale_color_gradient(low="#FFE300",high="#B20600")+
  geom_point() +
  labs(x = "Propiedades")
grafico_4

grafico_5 <- df_filtered |> ggplot(aes(c(1:nrow(df_filtered)), habitaciones, color=habitaciones)) +
  scale_color_gradient(low="#EBB02D",high="#159895")+
  geom_point() +
  labs(x = "Propiedades")
grafico_5

# Median
median(df_properties$valor)
# Mean
mean(df_properties$valor)

# Create the function to calculate the MODE
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
moda <- getmode(df_properties$valor)
moda




