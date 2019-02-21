#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!   Regresion Datos Panel   !!!!!!!!!!!!!!
#------------------------------------------------------------

k = as.numeric(input$producto)
# k=1
# periodos = c(2005,2007,2010,2015,2019)
periodos = c(2005,as.numeric(input$periodos),2019)

#Quitado del UI la opcion de eleccion del deflactor!!
mav12 = MedMovBeta(IPC$GENERAL,n=12)
SerieStnd = as.numeric( IPC[,k+1] / mav12$mvxRecup)

# Serie = mav12Gen$resxRecup
Fecha = as.Date(IPC$Fecha, format = "%d-%m-%y")
Anio = as.numeric(format(Fecha, "%Y"))


etiquetas = c()
for (i in 1:(length(periodos) - 1)) {
  etiquetas[i] = paste0("Periodo: ", periodos[i], " - ", periodos[i + 1])
}


PeriodoCorte = cut(Anio,
                   breaks = periodos ,
                   labels = etiquetas ,
                   right = F)


BDDgraf = data.frame(Fecha, SerieStnd , SerieOrig =  IPC[,k+1], IPC_GeneralS = mav12$mvxRecup , PeriodoCorte)
MediaSeries = BDDgraf %>% group_by(PeriodoCorte) %>% summarise(Media = mean (SerieStnd))

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Regresion: Modelo de Efectos Fijos -------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
BDDpanel = BDDgraf[,c("Fecha","SerieStnd","PeriodoCorte")]
BDDpanel$Tmp = 1:dim(BDDpanel)[1]
modelo1 = lm(data = BDDpanel, formula = SerieStnd ~ Tmp + PeriodoCorte*Tmp)

predicted = data.frame(Tmp=BDDpanel$Tmp, PeriodoCorte=BDDpanel$PeriodoCorte)
IPCfit = forecast(modelo1,predicted)
predicted$IPCfit = as.numeric(IPCfit$mean)
predicted$Fecha = BDDgraf$Fecha

