library(readxl)
library(tidyverse)
library(shiny)
library(shinyjs)
library(DT)

# Read Data
df_properties <- read_xlsx('properties_procesado.xlsx')
value_ini <- 100000
variables <- colnames(df_properties)
variables <- variables[c(-1,-2, -9, -10, -11)]

# to do linkeable the column named links 
linkeable <- function(x){
  toString(tags$a(href = x, x))
}
df_properties$links <- lapply(df_properties$links, linkeable)

#____Shiny App___
App <- function(){
  ui <- fluidPage(
    useShinyjs(),
    theme = bslib::bs_theme(bootswatch = "darkly"),
    titlePanel('', windowTitle = "PLUSVALIA_CRACKING"),
    HTML(r"(
         <h1 style="text-align:center">BIENES RAÍCES</h1>
         <h6 style="color:#FC2947;">
         <a style="text-decoration: none"
         target="_blank"
         href="https://raulaviles.netlify.app/">
         Designed by: Byron Raúl Avilés Rodríguez
         </a>
         </h6>
    )"),
    
    tags$head(
      tags$style(
        HTML("
      @media only screen and (max-width: 600px) {
        .responsive-table {
          width: 100% !important;
          font-size:80%;
        }
      }
      
      @media only screen and (min-width: 601px) {
        .responsive-table {
          width: 1000px !important;
          font-size:80%;
        }
      }
    ")
      )
    ),
    
    textOutput("panel"),
    tabsetPanel(
      id = "tabset",
      tabPanel("Propiedades",
               HTML(r"(<br>)"),
               sidebarLayout(
                 sidebarPanel(
                   HTML(r"(<br>)"),
                   checkboxGroupInput('var', 'Selección Variables', 
                                      choiceNames = list('valor',
                                                         'alícuota', 
                                                         'área', 
                                                         'habitaciones',
                                                         'baños',
                                                         'estacionamientos'
                                                          ),
                                      choiceValues = variables,
                                      ),
                   sliderInput('filt_habitaciones', 'Filtro por Habitaciones de Propiedades', value= c(0, max(df_properties$habitaciones)), min = 0, max = max(df_properties$habitaciones), sep=""),
                   sliderInput('filt_baños', 'Filtro por Baños de Propiedades', value= c(0, max(df_properties$baños)), min = 0, max = max(df_properties$baños), sep = ""),
                   sliderInput('filt_estacionamientos', 'Filtro por Estacionamientos de Propiedades', value= c(0, max(df_properties$estacionamientos)), min = 0, max = max(df_properties$estacionamientos), sep=""),
                   sliderInput('filt_alicuota', 'Filtro por Alícuota de Propiedades', value= c(0, max(df_properties$alicuota)), min = 0, max = max(df_properties$alicuota), sep=""),
                   numericInput('filt_valor', 'Ingresa el Valor USD de la Propiedad', value = round(median(df_properties$valor)), step = 1000),
                   numericInput('filt_area', 'Ingresa el área de la Propiedad', value = round(median(df_properties$area)), step = 1),
                 ),
                 mainPanel(
                   HTML(r"(
                     <h6 style="text-align:center">FILTRAR POR:</h6>
                   )"),
                   span(textOutput('titulo_graf'), style="text-align:center;
                        color:#00FFCA"),
                   plotOutput('plot_propiedades', brush = "plot_brush")
                 )
               ),
               fluidRow(
                 column(12, div(class = "responsive-table", DT::dataTableOutput("tabla_properties")))
               )
               
               ),
      tabPanel("Proyectos",
               )
    )
    
  )
  server <- function(input, output, session){
    Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
    thematic::thematic_shiny()
    
    output$panel <- renderText({
      paste("Pestaña Actual:", input$tabset)
    })
    
    hide('filt_valor')
    hide('filt_alicuota')
    hide('filt_area')
    hide('filt_habitaciones')
    hide('filt_baños')
    hide('filt_estacionamientos')
    
    # show and hide the input chossen
    observeEvent(input$var, {
      if('valor' %in% input$var){
        show('filt_valor')
      } else{
        hide('filt_valor')
      } 
      if ('alicuota' %in% input$var){
        show('filt_alicuota')
      } else {
        hide('filt_alicuota')
      }
      if ('area' %in% input$var){
        show('filt_area')
      } else {
        hide('filt_area')
      }
      if ('habitaciones' %in% input$var){
        show('filt_habitaciones')
      } else {
        hide('filt_habitaciones')
      }
      if ('baños' %in% input$var){
        show('filt_baños')
      } else {
        hide('filt_baños')
      }
      if ('estacionamientos' %in% input$var){
        show('filt_estacionamientos')
      } else {
        hide('filt_estacionamientos')
      }
      
    })
    
    # text of the filter input choseen and hide if the input var is null
    titulo_graf <- reactive(input$var)
    output$titulo_graf <- renderText({
      if(!is.null(input$var)){
        titulo_graf() 
      } else {
        hide('filt_valor')
        hide('filt_alicuota')
        hide('filt_area')
        hide('filt_habitaciones')
        hide('filt_baños')
        hide('filt_estacionamientos')
      }
      })
    
    # filter the tibble in fuction of all input filters
    data_1 <- reactive({
      req(input$filt_valor)
      req(input$filt_area)
      req(input$filt_alicuota)
      req(input$filt_estacionamientos)
      req(input$filt_baños)
      req(input$filt_habitaciones)
      if(!is.null(input$filt_valor) & !is.null(input$filt_area)){
        df_properties |> filter(.data[['valor']] <= input$filt_valor,
                                .data[['area']] <= input$filt_area,
                                .data[['alicuota']] >= input$filt_alicuota[1] & .data[['alicuota']] <= input$filt_alicuota[2],
                                .data[['estacionamientos']] >= input$filt_estacionamientos[1] & .data[['estacionamientos']] <= input$filt_estacionamientos[2],
                                .data[['baños']] >= input$filt_baños[1] & .data[['baños']] <= input$filt_baños[2],
                                .data[['habitaciones']] >= input$filt_habitaciones[1] & .data[['habitaciones']] <= input$filt_habitaciones[2],
                                ) |> mutate(numeracion = row_number()) |>
                                select(-c(any_of('total'))) |>
                                relocate(any_of('numeracion')) 
                                
      } else{
        df_properties
      } 
    })
    
    # plot the properties graph
    observeEvent(data_1(),
                 if(nrow(data_1())>0 & sum(data_1()$valor)>0){
                   output$plot_propiedades <- renderPlot({
                     req(data_1())
                     req(input$filt_valor)
                     req(input$filt_area)
                     req(input$filt_alicuota)
                     req(input$filt_estacionamientos)
                     req(input$filt_baños)
                     req(input$filt_habitaciones)
                     ggplot(data_1(), aes(numeracion, valor)) +
                       scale_color_gradient(low="#FFE300",high="#B20600")+
                       geom_point(aes(color= valor)) +
                       labs(x = "Propiedades", y = "Valor (USD)")
                   })
                   output$tabla_properties <- DT::renderDataTable({
                     brushedPoints(data_1(), 
                                   input$plot_brush
                                   )
                   }, rownames = FALSE,
                     options = list(
                       bSortClasses = TRUE,iDisplayLength = 10,   width = "100%",
                       scrollX=TRUE,
                       autoWidth = TRUE
                     )
                   )
                   
                 } else{
                   output$plot_propiedades <- NULL
                 }
                 )
    
  }
  shinyApp(ui, server)
}

App()

