# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

navbarPage(title = "Análisis IPC Ecuador",
           header = tags$h3(" - ",tags$head(tags$link(rel='shortcut icon', 
                                                      href='puce.ico', 
                                                      type='image/x-icon'))),
           position = "fixed-top",theme=shinytheme('yeti'),#theme = 'estilo.css',
           footer = fluidRow(column(12,img(src='puce_logo.png',width='90px',align='center'),
                                    tags$b('Proyecto: '),
                                    ' "Análisis del Poder de Mercado (Series IPC)".' ,
                                    '-',tags$a('Instituto de Investigaciones Económicas - PUCE (2018)',
                                               href='https://www.puce.edu.ec'),
                                    tags$b('  ||  '),tags$b('Desarrollado por: '),
                                    tags$a('Cristian Pachacama',
                                           href='http://www.linkedin.com/in/cristian-david-pachacama')
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

