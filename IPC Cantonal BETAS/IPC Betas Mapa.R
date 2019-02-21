library(highcharter)
library(dplyr)
library(rjson)
library(stringr)
library(readxl)
library(readr)
library(polynom)

library(xtable)
# Funciones Extras
source(file = "Code/Extrapolacion.R", local = TRUE)
source(file = "Code/MedMovil.R", local = TRUE)

#Carga de Datos --------------------------------------
load("Data/IPC_Cantonal.RData")
IPC_canton = IPC_canton_corre
remove(IPC_canton_corre)
#
ArchCodCanton = read.csv("Data/Mapa/ArchCodCanton.csv")
ArchCodCanton$COD_CANTON = substr(ArchCodCanton$ARCH_CANTON,start = 1,stop = 4)

# Periodos de Corte ==================================
periodos = c(2007,2010,2015)
periodos = c(2005,as.numeric(periodos),2019)
Fecha = as.Date(IPC_canton[[1]]$Fecha, format = "%Y-%m-%d")
Anio = as.numeric(format(Fecha, "%Y")) #;remove(Fecha)
etiquetas = c()
for (i in 1:(length(periodos) - 1)) {
  etiquetas[i] = paste0("Periodo: ", periodos[i], " - ", periodos[i + 1])
}
PeriodoCorte = cut(Anio,
                   breaks = periodos ,
                   labels = etiquetas ,
                   right = F)
remove(Anio,periodos,Fecha,i)

# Estandarización de Datos  ==========================
Productos = names(IPC_canton[[1]])

Productos[-1]

# Gama de Colores   ---------------------
library(viridisLite)
colorFuerte = "#5F0010"
colorSuave = "#FFF9DA"

n = 800
# viridis
# magma
# inferno
# cividis
colores = substring(inferno(n + 1), 0, 7)
lista_colores = data.frame(q = 0:n/n,
                           c = colores,
                           stringsAsFactors = FALSE)
lista_colores = list_parse2(lista_colores)
dl = 4



Productos[-1]

pd = 71  #producto   ---------------------

Productos[pd+1]

{
  
  
  
  #i = 1
  BDDMap = data.frame()
  for(i in 1:length(IPC_canton)){
    Fechas_aux = IPC_canton[[i]][,1] #Corregir
    mav12 = MedMovBeta(IPC_canton[[i]][,2],n=12)
    IPC_val_aux = IPC_canton[[i]][,pd+1]/as.numeric(mav12$mvxRecup) #IPC Deflactado
    nombre_aux = names(IPC_canton)[i]
    
    #Base del Canton i y producto pd  -------------------
    BDD_aux = data.frame(Periodo = PeriodoCorte,
                         Tmp = 1:length(Fechas_aux),
                         Valor = IPC_val_aux)      #mapa
    remove(mav12,Fechas_aux,IPC_val_aux)
    
    #Regresion de Panel  --------------------------------
    modelo = lm(data = BDD_aux, formula = Valor ~ Tmp*Periodo)
    remove(BDD_aux)
    # Betas del Modelo  ---------------------------------
    resumen = data.frame(round(xtable(summary(modelo)),digits = 5))
    names(resumen) = c("Estimación","Error Estándar","t-valor","Pr(>|t|)")
    Pval = as.numeric(summary(modelo)$coefficients[,4])
    rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
                 labels = c("***","**","*","."," "))
    
    resumen$Signif = rangos
    remove(Pval,rangos,modelo)
    
    b_nomb=startsWith(rownames(resumen),"Tmp")
    betas=resumen$`Estimación`[b_nomb]
    tablabetas=resumen[b_nomb,c(1,2)]
    remove(b_nomb)
    #Betas Acumulados (Sumado el pivote)
    tablabetas[2:length(tablabetas[,1]),1]=tablabetas[2:length(tablabetas[,1]),1]+tablabetas[1,1]
    tablabetas = data.frame(ARCH_CANTON = nombre_aux, #mapa
                            Fecha = as.character(etiquetas), 
                            Valor = tablabetas[,1])
    #Datos para MAPA  ----------------------------------
    BDDMap = rbind(BDDMap, tablabetas)
  }
  
  BDDMap = ArchCodCanton %>%
    inner_join(BDDMap,by="ARCH_CANTON") %>%
    select(COD_CANTON,CANTON,Fecha,Valor)
  
  
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #-------------------------  MAPA ANIMADO  --------------------------
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #Data para Animacion Mapa
  producto = Productos[pd+1]
  print(paste("*************",producto,"*************"))
  MapaCanton = fromJSON(file = "Data/Mapa/Mapa_Cantonal.geojson")
  
  DFMap = BDDMap %>% 
    group_by(COD_CANTON) %>% 
    do(item = list(
      COD_CANTON = first(.$COD_CANTON),
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
                  mapData = MapaCanton,
                  name = "IPC deflactado",
                  value = "Valor", 
                  joinBy = c("DPA_CANTON", "COD_CANTON"),
                  dataLabels = list(enabled = TRUE,
                                    format = '{point.properties.DPA_DESCAN}')) %>%
    
    # hc_colorAxis(dataClasses = color_classes(seq(25124, 3645483, length.out = 12))) %>%
    
    # hc_colorAxis(minColor = "#922B21", min= min(BDDMap$Valor),
    #              maxColor = "#F9EBEA", max= max(BDDMap$Valor))  %>%
    
    # hc_colorAxis(minColor = colorFuerte,
    #              maxColor = colorSuave)  %>%
    
    
    # Usando Lista de Colores Paleta "Viridis"  --------------
    hc_colorAxis(stops = lista_colores,
                 # type = "logarithmic",
                 min= min(BDDMap$Valor)+dl*(10^(-4)),
                 max= max(BDDMap$Valor)-dl*(10^(-4))  ) %>%
    
    
    
    # hc_colorAxis(dataClasses = color_classes(breaks = seq(25124, 3645483, length.out = 40),
    #                                          colors = c("#ff0000", "#ffcc00", "#33cc33"))) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    #!!!!!!!!!!!!!!!!       Temas      !!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # hc_add_theme(hc_theme_ffx()) %>%
    
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
        step = 5*(10^(-3)) #Velocidad !!!!!!!!!!!
      )
    )
  
  
  
  
}
  



