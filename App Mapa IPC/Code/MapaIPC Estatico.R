#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------------  MAPA Estático --------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# MapaProv = fromJSON(file= "Data/Mapa/Mapa_Ecuador.json")
# PROVprov = read_excel("Data/Mapa/PROV_prov.xlsx")

DatosMapa_est = reactive({
  
  period = as.character(input$fch)
  period = paste0(substring(period,first = 1,last = 8),"01")
  # fcha = as.Date(period)
  fcha = period
  remove(period)
  
  pd = as.numeric(input$producto_est)  #producto
  # productos = names(IPC_prov[[1]])
  
  BDDMap = data.frame()
  
  # IPC Original  ----------------------------
  if(input$tipo_serie == 1){
    
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
      select(Fecha,Provincia,Valor) %>% 
      filter(Fecha==fcha)
    
  }
  
  # IPC Deflactado  --------------------------
  if(input$tipo_serie == 2){
    
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
      select(Fecha,Provincia,Valor) %>% 
      filter(Fecha==fcha)
    
  }
  
  return(BDDMap)
})


# Mapa Estático  ----------------------------------
output$mapaIPC_est = renderHighchart({
  
  fcha = input$fch
  BDDMap = DatosMapa_est()# %>% select(Provincia,Valor)
  pd = as.numeric(input$producto_est)
  prod = productos[pd]
  
  if(input$tipo_serie == 1) etiqueta = "IPC"
  if(input$tipo_serie == 2) etiqueta = "IPC Deflactado"
  
  print(paste("**********  Producto: ",prod,"  *****  ","Fecha:",fcha," *****"))
  
  #Mapa
  mapa_est = highchart() %>%
    # hc_title(text = "<b>Indice de Precios al Consumidor</b>",
    #          margin = 20, align = "center") %>% 
    # hc_subtitle(text = prod,
    #             align = "center") %>% 
    hc_tooltip(followPointer =  TRUE) %>%
    hc_add_series_map(map = MapaProv, 
                      df = BDDMap, 
                      name = etiqueta,
                      value = "Valor", 
                      joinBy = c("name", "Provincia"),
                      dataLabels = list(enabled = TRUE,
                                        format = '{point.properties.woe-name}')
                      ) %>%
    hc_colorAxis(minColor = "#922B21", min=min(BDDMap$Valor),
                 maxColor = "#F9EBEA",max=max(BDDMap$Valor)) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_legend(layout = "vertical", align = "left", reversed = TRUE,
              verticalAlign = "top", floating = TRUE,  x = 30, y = 300 )
    # hc_legend(enabled = FALSE)
  # print(mapa_est)
  mapa_est
})



