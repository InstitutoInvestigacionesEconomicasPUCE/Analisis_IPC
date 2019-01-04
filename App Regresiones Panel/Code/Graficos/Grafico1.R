#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!  GRAFICAS INDIVIDUALES 1  !!!!!!!!!!!!!!
#------------------------------------------------------------
# Graficos del IPC sin deflactar, Regresion Simple
Pval = as.numeric(summary(modelo1)$coefficients[,4])
rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
             labels = c("***","**","*","."," "))

betaAux = round(as.numeric(coef(modelo1)[2]),digits = 3)
graf=ggplot(data = BDDgraf, aes(x = Fecha, y = SerieOrig)) +
  geom_line(size = 0.7,colour = "black") +  
  geom_smooth(method = "lm",color="red") +
labs(title = paste("IPC & Regresión:", productos[k]) , y = "IPC")+
  annotate("text", 
           label = TeX(paste0("$ \\beta = ",
                              betaAux,"$",
                              as.character(rangos[2])
           )
           ), 
           x = as.Date("01-01-2010", format = "%d-%m-%Y"), 
           y = max(BDDgraf$SerieOrig),
           size = 7
  ) + 
  scale_x_date(                                        
    breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  theme(axis.text.x=element_text(angle=90))

plot(graf)

