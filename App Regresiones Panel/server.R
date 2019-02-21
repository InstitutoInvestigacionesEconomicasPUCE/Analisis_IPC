# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
function(input, output,session) {
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

