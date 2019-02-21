library(highcharter)
library(dplyr)
library(rjson)
library(stringr)
library(readxl)
#Carga de Datos --------------------------------------
load("IPC_Provincial.RData")
IPC_prov = IPC_prov_corre
remove(IPC_prov_corre)

PROVprov = read_excel("Mapa/PROV_prov.xlsx")
MapaProv = fromJSON(file= "Mapa/Mapa_Ecuador.json")

# Estandarización de Datos  ==========================
pd = 92  #producto
Productos = names(IPC_prov[[1]])

#i = 1
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


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------------------  MAPA ANIMADO  --------------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Data para Animacion Mapa
# MapaCanton = fromJSON(file = "https://raw.githubusercontent.com/Rusersgroup/mapa_ecuador/master/ec-all.geo.json")
producto = Productos[pd+1]
# BDDMap = read_excel("Data/Mapa/Mapa_Data_Ej.xlsx")  #!!!!!!
# glimpse(MapaProv)

DFMap = BDDMap %>% 
  group_by(Provincia) %>% 
  do(item = list(
    Provincia = first(.$Provincia),
    sequence = .$Valor,
    value = first(.$Valor))) %>% 
  .$item

#----------------------------------------------------------
highchart(type = "map") %>%
  hc_title(text = "<b>Índice de Precios al Consumidor</b>",
           margin = 20, align = "center") %>% 
  hc_subtitle(text = producto,
              align = "center") %>% 
  hc_tooltip(followPointer =  TRUE) %>%
  hc_add_series(data = DFMap,
                mapData = MapaProv,
                name = "Población",
                value = "Valor", 
                joinBy = c("name", "Provincia"),
                dataLabels = list(enabled = TRUE,
                                  format = '{point.properties.woe-name}')) %>%
  
  # hc_motion(enabled = TRUE,series = 1,labels = 1:n) %>% 
  # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
  hc_colorAxis(minColor = "#F9EBEA", maxColor = "#922B21")  %>%
  # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 40),
  #                                          colors = c("#ff0000", "#ffcc00", "#33cc33"))) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  hc_add_theme(hc_theme_ffx()) %>%
  # hc_add_theme(hc_theme_darkunica()) %>%
  # hc_add_theme(hc_theme_economist()) %>%
  # hc_chart(borderColor = "#404040",
  #          borderRadius = 10,
  #          borderWidth = 2) %>%
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_motion(
    enabled = TRUE,
    axisLabel = "Fecha",
    labels = sort(unique(BDDMap$Fecha)),
    series = 0,
    updateIterval = 10,
    magnet = list(
      round = "floor",
      step = 0.17 #Velocidad !!!!!!!!!!!
    )
  )
