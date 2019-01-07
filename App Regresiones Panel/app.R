#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

# Analisis de : IPC (Serie Empalmada Regional)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------        ANALISIS Regresion IPC - PUCE      ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
library(latex2exp)
#Tablas
library(readxl)
library(readr)
library(curl)
library(xtable)
library(DT)
library(tidyverse)
#Series de Tiempo
library(polynom)
library(TSdist)
library(ggplot2)
library(gridExtra)
# library(xts)
library(forecast)
library(hexbin)
library(MASS)
#>> Carga de Datos -----------------------------------------------
# http://www.ecuadorencifras.gob.ec/series-empalmadas-ipc-base-2004/
# IPC = read_csv("Data/IPChistorico.csv")

source(file ="Code/CargaData.R" ,local = TRUE)

productos = names(IPC)[-1]
ProductosLista = 1:length(productos)
names(ProductosLista) = paste(ProductosLista,".",productos)
# IPC = data.frame(IPC)
# names(IPC) = c("Fecha", productos)

PeriodoLista = 2006:2018
names(PeriodoLista) = paste("Año:",PeriodoLista)

#Tipo de Estandarizacion (Deflactar)
TipoDeflactor = c("MM12(IPC General)" = 1, "MM12(Serie Original)" = 2)

#-------------------------------------------
source(file ="Code/MedMovil.R" ,local = TRUE)
#------------------------------------------------

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
                              p('Para el anális se considera el Índice de Precios al 
                                Consumidor de todos los bienes de la canasta básica 
                                familiar (series mensuales de los últimos 13 años).
                                Podemos encontrar las series de los siguientes 116 productos.
                                Se procedió a realizar el análisis Clúster de las series 
                                asociadas a estos productos, para el análisis se consideraron 
                                varias métricas, entre las que se destacan las que consideran la correlación 
                                tanto temporal como entre distintas series.')#,
                              # fluidRow(dataTableOutput("tabla_prod"))
                              
                            )
                            
                            
                            
                          ),hr()
                          
                          
                 ),
                 
                 #INFORMACIÓN DE LA BASE DE DATOS ------------------------------
                 # tabPanel("Datos"
                 #            
                 #            ),
                 
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 tabPanel('Análisis',
                          
                          fluidRow(
                            # Panel Lateral -------------------------------
                            sidebarPanel(width = 3,
                                         h4('Panel Control Graficos'),
                                         selectInput('region', 
                                                     label= 'Selecciona Nivel',
                                                     selected = 1,
                                                     choices=c("NACIONAL"=1,
                                                               "REGIÓN SIERRA"=2,
                                                               "REGIÓN COSTA"=3,
                                                               "Guayaquil"=4,
                                                               "Esmeraldas"=5,
                                                               "Machala"=6,
                                                               "Manta"=7,
                                                               "Santo Domingo"=8,
                                                               "Quito"=9,
                                                               "Loja"=10,
                                                               "Cuenca"=11,
                                                               "Ambato"=12)
                                         ),
                                         selectInput('tipoAnalisis', 
                                                     label= 'Selecciona Índice',
                                                     selected = 1,
                                                     choices=c("IPC sin deflactar"=1,
                                                               "IPC deflactado"=2,
                                                               "IPC Deflactado, periodos de corte"=3
                                                               )
                                         ),
                                         selectInput('producto', 
                                                     label= 'Selecciona Producto',
                                                     selected = 2,
                                                     choices=ProductosLista),
                                         
                                         # checkboxGroupInput("periodos", 
                                         #                    label = "Eligir Periodos de Corte", 
                                         #                    choices = PeriodoLista,
                                         #                    selected = c(2007,2010,2015))
                                         uiOutput("moreControls")
                            ),
                            # Panel Central ------------------------------------
                            mainPanel(
                              # h3('Distribución del IPC Deflactado'),
                              h3(textOutput('titulo')),
                              h4(textOutput('productNombre')),hr(),
                              plotOutput('graficoDist',height = "530px",width = '110%'),
                              h4('Resumen del Modelo'),
                              DTOutput('resumenRegres')
                              
                            )
                          ),hr()
                 )
)


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  #Carga de Datos Reactiva --------------------
  source(file = "Code/DataReactiva.R", local = TRUE)
  
  #Titulo -------------------------------------
  output$titulo = renderText({
    TipoAnalisis = c("IPC sin deflactar"=1,
                     "IPC Deflactado"=2,
                     "IPC Deflactado (Análisis por periodos)"=3
                     )
    names(TipoAnalisis)[as.numeric(input$tipoAnalisis)]
    
  })
  
  
  #Subtitulo Producto  ------------------------
  output$productNombre = renderText({
    
    names(ProductosLista)[as.numeric(input$producto)]
    
  })
  output$moreControls <- renderUI({
    if(input$tipoAnalisis == 3){
      checkboxGroupInput("periodos", 
                         label = "Eligir Periodos de Corte", 
                         choices = PeriodoLista,
                         selected = c(2007,2010,2015))}
  }
  )
  
  # Grafico Generado ---------------------------
  output$graficoDist = renderPlot({
    IPC = IPCreact()
    #Datos Iniciales y Analisis ----------------
    k = as.numeric(input$producto)
    periodos = c(2005,as.numeric(input$periodos),2019)
    
    # Betas del IPC Sin deflactar
    if(input$tipoAnalisis == 1){
      #Regresion   
      source(file = "Code/Regresiones/Regresion.R", local = TRUE)
      #Graficos Individuales 
      source(file = "Code/Graficos/Grafico1.R", local = TRUE)
      
    }
    
    # Betas del IPC Deflactado
    if(input$tipoAnalisis == 2){
      #Regresion   
      source(file = "Code/Regresiones/RegresionDeflactado.R", local = TRUE)
      #Graficos Individuales 
      source(file = "Code/Graficos/Grafico2.R", local = TRUE)
    }
    
    # Betas del IPC Deflactado, cortes en Periodos
    if(input$tipoAnalisis == 3){
      
      try({
        #Regresion   
        source(file = "Code/Regresiones/RegresionPanel.R", local = TRUE)
        #Graficos Individuales 
        source(file = "Code/Graficos/Grafico3.R", local = TRUE)
      })
      
     
    }
    
    #Fijado periodo, betas del IPC de Productos
    if(input$tipoAnalisis == 4){
      #Regresion   
      source(file = "Code/Regresiones/RegresionPanel.R", local = TRUE)
      #Graficos Individuales 
      source(file = "Code/Graficos/Grafico4.R", local = TRUE)
    }
   
    
  })
  
  
  # Tabla resumen de Regresion  -----------------------------
  output$resumenRegres = renderDT({
    IPC = IPCreact()
    #Tabla 1:     
    if(input$tipoAnalisis == 1){
      source(file = "Code/Regresiones/Regresion.R", local = TRUE)
      source(file = "Code/Tablas/Tabla1.R", local = TRUE)
      
    }
    
    #Tabla 2: 
    if(input$tipoAnalisis == 2){
      source(file = "Code/Regresiones/RegresionDeflactado.R", local = TRUE)
      source(file = "Code/Tablas/Tabla2.R", local = TRUE)
    }
    
    #Tabla 3: 
    if(input$tipoAnalisis == 3){
      
      try({
        source(file = "Code/Regresiones/RegresionPanel.R", local = TRUE)
        source(file = "Code/Tablas/Tabla3.R", local = TRUE)
      })
      
    }
    
    #Tabla 4: 
    if(input$tipoAnalisis == 4){
      source(file = "Code/Tablas/Tabla4.R", local = TRUE)
    }
    
    #Compila Tabla generada en Tabla_i
    ResumenTabla
  })
  
  
}


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)

