#------------------------------------------------------------
#!!!!!!!!!!!!    Tabla Regresion Panel IPC     !!!!!!!!!!!!!!
#------------------------------------------------------------

#Datos Iniciales y Analisis -----------------------------


#Tabla Resumen de Regresion -----------------------------
resumen = data.frame(round(xtable(summary(modelo1)),digits = 5))
names(resumen) = c("Estimación","Error Estándar","t-valor","Pr(>|t|)")
Pval = as.numeric(summary(modelo1)$coefficients[,4])
rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
             labels = c("***","**","*","."," "))
resumen$Signif = rangos


ResumenTabla = datatable(#filter = 'top',
  #Formato de la tabla -------------------
  extensions = c('Buttons'), #c('Responsive','Buttons'),
  options = list(pageLength=10,searchHighlight = TRUE,
                 dom = 'Bfrtip',
                 buttons = list('copy','print', list(
                   extend = 'collection',
                   buttons = c('csv', 'excel', 'pdf'),
                   text = 'Descargar'
                 ))
  ),
  {
    #Tabla a Mostrar  --------------------
    resumen
  }
)

