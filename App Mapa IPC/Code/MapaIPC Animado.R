#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------------  MAPA ANIMADO  --------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# MapaProv = fromJSON(file= "Data/Mapa/Mapa_Ecuador.json")
# PROVprov = read_excel("Data/Mapa/PROV_prov.xlsx")

# Estandarización de Datos  ==========================
DatosMapa_Anim = reactive({
  
  pd = as.numeric(input$producto_est)  #producto
  # productos = names(IPC_prov[[1]])
  
  # IPC Original  ----------------------------
  if(input$tipo_serie == 1){
    BDDMap = data.frame()
    for(i in 1:length(IPC_prov)){
      Fechas_aux = IPC_prov[[i]][,1] #Corregir
      IPC_val_aux = IPC_prov[[i]][,pd+1]
      nombre_aux = names(IPC_prov)[i]
      BDDMap_aux = data.frame(PROVINCIA = nombre_aux,
                              Fecha=Fechas_aux, 
                              Valor = IPC_val_aux)
      BDDMap = rbind(BDDMap, BDDMap_aux)
    }
    
    BDDMap = PROVprov %>% 
      inner_join(BDDMap,by="PROVINCIA") %>% 
      select(Provincia,Fecha,Valor)
    
  }
  
  # IPC Deflactado  --------------------------
  if(input$tipo_serie == 2){
    BDDMap = data.frame()
    for(i in 1:length(IPC_prov)){
      Fechas_aux = IPC_prov[[i]][,1] #Corregir
      mav12 = MedMovBeta(IPC_prov[[i]][,2],n=12) #Media Movil del IPC Genral
      IPC_val_aux = IPC_prov[[i]][,pd+1]/as.numeric(mav12$mvxRecup) #IPC Deflactado
      nombre_aux = names(IPC_prov)[i]
      BDDMap_aux = data.frame(PROVINCIA = nombre_aux,
                              Fecha=Fechas_aux, 
                              Valor = IPC_val_aux)
      BDDMap = rbind(BDDMap, BDDMap_aux)
    }
    
    BDDMap = PROVprov %>% 
      inner_join(BDDMap,by="PROVINCIA") %>% 
      select(Provincia,Fecha,Valor)
  }
  
  #Data para Animacion Mapa 
  # prod = productos[pd]
  # print(paste("**********  Producto:",prod,"  ***********"))
  
  DFMap = BDDMap %>% 
    group_by(Provincia) %>% 
    do(item = list(
      Provincia = first(.$Provincia),
      sequence = .$Valor,
      value = first(.$Valor))) %>% 
    .$item
  
  aux = list("BDDMap"=BDDMap,"DFMap"=DFMap)
  return(aux)
})


#Mapa HighCharter   ===================================
output$mapaIPC = renderHighchart({
  
  BDDMap = DatosMapa_Anim()$BDDMap
  DFMap = DatosMapa_Anim()$DFMap
  pd = as.numeric(input$producto_est)
  prod = productos[pd]
  
  if(input$tipo_serie == 1) etiqueta = "IPC"
  if(input$tipo_serie == 2) etiqueta = "IPC Deflactado"
  
  mapa = highchart(type = "map") %>%
    hc_title(text = "<b>Indice de Precios al Consumidor</b>",
             margin = 20, align = "center") %>% 
    hc_subtitle(text = prod,
                align = "center") %>% 
    hc_tooltip(followPointer =  TRUE) %>%
    hc_add_series(data = DFMap,
                  mapData = MapaProv,
                  name = "IPC",
                  value = "Valor", 
                  joinBy = c("name", "Provincia"),
                  dataLabels = list(enabled = TRUE,
                                    format = '{point.properties.woe-name}')) %>%
    # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
    hc_colorAxis(minColor = "#922B21", maxColor = "#F9EBEA")  %>%
    # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 40),
    #                                          colors = c("#ff0000", "#ffcc00", "#33cc33"))) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # hc_add_theme(hc_theme_ffx()) %>%
    # hc_add_theme(hc_theme_darkunica()) %>%
    # hc_add_theme(hc_theme_economist()) %>%
    # hc_chart(borderColor = "#404040",
    #          borderRadius = 10,
    #          borderWidth = 2) %>%
    hc_legend(layout = "vertical", align = "left", reversed = TRUE,
              verticalAlign = "top", floating = TRUE,  x = 30, y = 300 ) %>% 
    hc_motion(
      enabled = TRUE,
      axisLabel = "Fecha",
      labels = sort(unique(BDDMap$Fecha)),
      series = 0,
      updateIterval = 10,
      magnet = list(
        round = "floor",
        step = 0.11 #Velocidad !!!!!!!!!!!
      )
    )
  
  print(mapa)
  
})

