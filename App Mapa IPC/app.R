#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------       MAPA IPC Provincial - PUCE 2019     ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
library(latex2exp)
library(fontawesome)
library(htmltools)
library(htmlwidgets)
#Tablas
library(readxl)
library(readr)
# library(DT)
library(tidyverse)
library(stringr)
#Graficos
library(highcharter)
library(rjson)
#Extrapolacion
library(polynom)

#>> Carga de Datos -----------------------------------------------
source(file ="Code/CargaData.R" ,local = TRUE)
source(file = "Code/Extra/MedMovil.R",local = TRUE)
source(file = "Code/Extra/Extrapolacion.R", local = TRUE)

#Lista Productos Inicial
productos = names(IPC_prov[[1]])[-1]
ProductosLista = 1:length(productos)
names(ProductosLista) = paste(ProductosLista,".",productos)

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

ui <- navbarPage(title = "Análisis IPC Ecuador",
                 header = tags$h3(" - ",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='puce.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",theme=shinytheme('yeti'),#theme = 'estilo.css',
                 footer = fluidRow(column(12,img(src='puce_logo.png',width='90px',align='center'),
                                          tags$b('Proyecto: '),' "Análisis del Poder de Mercado (Series IPC)".' ,
                                          '-',tags$a('Instituto de Investigaciones Económicas - PUCE (2018)',href='https://www.puce.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('Cristian Pachacama',href='http://www.linkedin.com/in/cristian-david-pachacama')
                 )
                 ),
                 
                 #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
                 tabPanel('Introducción',icon=icon('home'),
                          
                          fluidRow(
                            
                            sidebarPanel(img(src='puce_logo2.png',width='90%',align='center' ),
                                         fluidRow(' '),
                                         hr(),
                                         fluidRow(
                                           column(3,tags$b('Proyecto:')),column(1),
                                           column(8,'Análisis del Índice de Precios al Consumidor - Ecuador')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Linea de Investigación:')),column(1),
                                           column(8,'Econometría')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Unidad:')),column(1),
                                           column(8,'Instituto de Investigaciones Económicas')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Director:')),column(1),
                                           column(8,'PhD. Pedro Páez')
                                         ),hr(),
                                         fluidRow(
                                           column(3,tags$b('Researcher:')),column(1),
                                           column(8,'Cristian Pachacama')
                                         )
                                         
                            ),
                            
                            mainPanel(
                              h3('Análisis del Índice de Precios al Consumidor'),
                              hr(),h4('Periodo Enero 2005 - Noviembre 2018'),
                              fluidRow(' '),
                              p('Para el análisis se considera el Índice de Precios al 
                                Consumidor de todos los bienes de la canasta básica 
                                familiar (series mensuales de los últimos 13 años).
                                Podemos encontrar las series de los siguientes 116 productos.
                                Se procedió a realizar el análisis Clúster de las series 
                                asociadas a estos productos, para el análisis se consideraron 
                                varias métricas, entre las que se destacan las que consideran la correlación 
                                tanto temporal como entre distintas series.')#
                              
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 
                 # MAPA HISTORICO DEL IPC ============================
                 
                 # Mapa ESTATICO  ======================
                 tabPanel('Mapa',
                          
                          fluidRow(
                            # Panel Lateral -------------------------------
                            sidebarPanel(width = 3,
                                         h4('Panel Control Graficos'),
                                         
                                         p("Seleccion el nivel de desagregación del mapa."),
                                         selectInput(inputId = 'tipo_mapa', 
                                                     label= h5('Mapa'),
                                                     selected = 1,
                                                     choices=c("Provincial" = 1,
                                                               "Cantonal" = 2)),
                                         
                                         p("Seleccione el item que desea analizar."),
                                         selectInput(inputId = 'producto_est', 
                                                     label= h5('Producto'),
                                                     selected = 2,
                                                     choices=ProductosLista),
                                         
                                         p("Que datos desea Analizar?"),
                                         selectInput(inputId = 'tipo_serie', 
                                                     label= h5('Serie'),
                                                     selected = 1,
                                                     choices=c("Serie IPC"=1,
                                                               "Serie IPC Deflactado" = 2)),
                                         
                                         p("A continuación seleccione una fecha (solo se 
                                           cuenta con un dato por mes)"),
                                         dateInput(inputId = "fch", 
                                                   label = h5("Fecha"), 
                                                   value = "2018-11-01",
                                                   min = "2004-01-01",
                                                   max = "2018-12-01",
                                                   startview = "year",
                                                   format = "yyyy-mm")
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              
                              h3("Índice de Precios al Consumidor"),
                              h4(textOutput('productNombre_est')),
                              h5(textOutput('periodo_est')),hr(),
                              highchartOutput("mapaIPC_est",height = "600px"),
                              hr(),
                              
                              # Mapa Animado (Segundo Plano)  ------------------
                              highchartOutput("mapaIPC",height = "600px")
                              
                            )
                          ),hr()
                 )#,
                 
                 
                 # Mapa ANIMADO  =======================
                 # tabPanel('Mapa Animado',
                 #          
                 #          fluidRow(
                 #            # Panel Lateral -------------------------------
                 #            sidebarPanel(width = 3,
                 #                         h4('Panel Control Graficos'),
                 #                         
                 #                         selectInput('producto', 
                 #                                     label= 'Selecciona Producto',
                 #                                     selected = 2,
                 #                                     choices=ProductosLista)
                 #            ),
                 #            # Panel Central ------------------------------------
                 #            mainPanel(
                 #              
                 #              h3("Índice de Precios al Consumidor"),
                 #              h4(textOutput('productNombre')),
                 #              h5("[Periodo: Enero 2005 - Diciembre 2018]"),hr(),
                 #              highchartOutput("mapaIPC",height = "600px")
                 #              
                 #            )
                 #          ),hr()
                 # )
)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  #Subtitulo Producto   ------------------------
  output$productNombre_est = renderText({
    names(ProductosLista)[as.numeric(input$producto_est)]
  })
  output$periodo_est = renderText({
    period = as.character(input$fch)
    # period = paste0(substring(period,first = 1,last = 8),"01")
    period = substring(period,first = 1,last = 7)
    paste("[Fecha:",period,"]")
  })
  #Subtitulo Producto 2 ------------------------
  output$productNombre = renderText({
    
    names(ProductosLista)[as.numeric(input$producto)]
    
  })
  
  # Mapas  PROVINCIAL  ----------------------
  source(file="Code/MapaIPC Estatico.R",local = TRUE)
  source(file="Code/MapaIPC Animado.R",local = TRUE)
  
  
  # Mapas CANTONAL  -------------------------
  
  
  
  
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

