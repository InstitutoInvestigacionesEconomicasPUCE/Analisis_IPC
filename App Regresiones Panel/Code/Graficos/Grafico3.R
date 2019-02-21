#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!  GRAFICAS INDIVIDUALES 3  !!!!!!!!!!!!!!
#------------------------------------------------------------
# Graficos del IPC Deflactado, Regresion Panel
# k=2
anhos=function(periodos){
  
  periodos[2:length(periodos)]=periodos[2:length(periodos)]+1
  periodos=paste0("01-06-",periodos)
  periodos=as.Date(periodos,format = "%d-%m-%Y")
  return(periodos[1:length(periodos)-1])
}
perd=anhos(periodos)
#---------------------------------------
BDDgraf1 = BDDgraf
resumen = data.frame(round(xtable(summary(modelo1)),digits = 5))
names(resumen) = c("Estimación","Error Estándar","t-valor","Pr(>|t|)")
Pval = as.numeric(summary(modelo1)$coefficients[,4])
rangos = cut(Pval,breaks = c(0,0.001,0.01,0.05,0.1,1),
             labels = c("***","**","*","."," "))
resumen$Signif = rangos
x=startsWith(rownames(resumen),"Tmp")
betas=resumen$`Estimación`[x]
tablabetas=resumen[x,c(1,2)]

#---------------------------------------
tablabetas[2:length(tablabetas[,1]),1]=tablabetas[2:length(tablabetas[,1]),1]+tablabetas[1,1]
N=300
etiquetas1 = c()
for (i in 1:(length(periodos) - 1)) {
  etiquetas1=c(etiquetas1,replicate(N,etiquetas[i]))
}
etiquetas1=as.factor(etiquetas1)

# periodos = c(2010,2011,2012,2013)
xsim=c()
for (i in 1:length(tablabetas[,1])) {
  xsim=c(xsim,rnorm(N,tablabetas[i,1],tablabetas[i,2]))
}
BDDFF=data.frame(Periodos_Corte = etiquetas1,xsim)  
mediasbetas=data.frame(medias=tablabetas[,1], periodos1=etiquetas)
BDDgraf1 = BDDgraf1[,c(1,3,4)]
names(BDDgraf1) = c("Fecha",
                    "Serie Original",
                    "MM12(IPC General)")

BDDgraf1 = BDDgraf1 %>%
  gather(key = "Serie", value = "value", -Fecha)
#-------------------------------------------
seriegraf1 = ggplot(BDDgraf1, aes(x = Fecha, y = value)) + 
  geom_line(aes(color = Serie), size = 0.7) +
  scale_color_manual(values = c("#0174DF","#2E2E2E")) +
  theme_minimal()+
  labs(title = paste("IPC:", productos[k]) , y = "IPC") +
  geom_vline(
    xintercept = as.Date(paste0(periodos[-c(1,length(periodos))],"-01-01")),
    linetype = "dashed",
    color = "red",
    size = 1
  ) +
  theme(
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.text.x=element_text(angle=90)
  )+ 
  scale_x_date(                                        
    breaks = "4 months",
    date_labels = "%b %Y"
  )
#----------------------------------------
posY = 0.92
seriegraf2 =  ggplot(data = BDDgraf, aes(x = Fecha, y = SerieStnd)) +
  geom_line(size = 0.7) + theme_minimal() +
  labs(title = paste("IPC Deflactado & Regresión por Periodo:", productos[k]) , 
       y = "IPC Deflactado") +
  geom_vline(
    xintercept = as.Date(paste0(periodos[-c(1, length(periodos))], "-01-01")),
    linetype = "dashed",
    color = "red",
    size = 1,
    labels()
  ) +
  geom_line(
    data = predicted,  #Anadir Lineas de Regresion !!!!!!!!!
    aes(x = Fecha, y = IPCfit, colour = PeriodoCorte),
    size = 0.7
  ) +
  annotate("text",
           label = TeX(paste0("$ \\beta = ",
                              round(betas,5),"$",
                              as.character(rangos[x]))
           ),
           x = perd,
           # x = as.Date("01-01-2010", format = "%d-%m-%Y"), 
           y = max(BDDgraf$SerieStnd)*1.07,
           size = 4
  )+
  annotate("text",
           label = TeX(paste0("$ \\alpha = ",
                              round(mediasbetas$medias,5),"$"
           )
           ),
           x = perd,
           #Posicion Y
           y = posY*max(BDDgraf$SerieStnd)*1.07 + (1-posY)*min(BDDgraf$SerieStnd),
           size = 4
  )+
  theme(
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.justification = c(0, 1),
    legend.position = c(0.75,0.5),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.text.x=element_text(angle=90)
  )+ 
  scale_x_date(                                        
    breaks = "4 months",
    date_labels = "%b %Y"
  )


#Grafico Densidades  ------------------------------------
densidades = ggplot(data = BDDFF ,
                    aes(xsim, fill = Periodos_Corte, color=Periodos_Corte)) +  
  geom_density(alpha = 0.2) +
  labs(title = TeX("Distribución de $\\alpha 's$ por Periodo")) +
  geom_vline(data = mediasbetas,
             aes(xintercept = medias, color=periodos1),
             linetype = "dashed") +
  theme(
    legend.title = element_text(size = 12, color = "black", face = "bold"),
    legend.justification = c(0, 1),
    legend.position = c(0.05, 0.95),
    legend.background = element_blank(),
    legend.key = element_blank()
  ) +
  annotate("text",
           label = TeX(paste0("$ \\alpha = ",
                              round(mediasbetas$medias,5),"$"
           )
           ),
           x = mediasbetas$medias,
           #Posicion Y
           y = -10,
           size = 3
  )


# Densidad de Residuos ???
# Ultimo Periodo
# Incluye Linea VaR (Valor en Riesgo)   ---------------------------------
# ResidPanel = data.frame(Fecha, PeriodoCorte, y = modelo1$residuals)
# ResidPanel = ResidPanel %>% filter(PeriodoCorte == etiquetas[length(etiquetas)])
# VaR = quantile(ResidPanel$Residuos,0.05)
# VarGraf = ggplot(data = ResidPanel, aes(y)) +
#   geom_density(alpha = 0.2, size=1.2) +
#   labs(title = TeX("Distribucion: Residuos del ultimo periodo")) +
#   geom_vline(aes(xintercept = VaR),
#              linetype = "dashed",
#              colour = "red",
#              size = 1.5)

# Distribución del IPC deflactado
BVar = BDDpanel %>%
  dplyr::filter(PeriodoCorte == etiquetas[length(etiquetas)]) %>% 
  dplyr::select(Fecha,PeriodoCorte,SerieStnd)
BVar = with(density(BVar$SerieStnd), data.frame(x, y))
VaR = quantile(BVar$SerieStnd,0.05)

al=0.3;al2=0.23;
VarGraf = ggplot(data = BVar, mapping = aes(x = x, y = y)) +
  labs(title = TeX("Distribucion: IPC Deflactado (ultimo periodo)")) +
  geom_line(size=1,colour="grey")+
  geom_area(mapping = aes(x = ifelse(x< VaR , x, NA)), fill = "red",alpha=0.4) +
  geom_area(mapping = aes(x = ifelse(x> VaR , x, NA)), fill = "#33cccc",alpha=0.4) +
  ylim(0,max(BVar$y)) + ylab("Densidad")+
  xlim(min(BVar$x), max(BVar$x)) + xlab("IPC Deflactado") +
  geom_vline(aes(xintercept = VaR),
             linetype = "dashed",
             colour = "red",
             size = 1.2)+
  annotate("text",
           label = "5% VaR",
           x = al*min(BVar$x)+(1-al)*VaR,
           #Posicion Y
           y = al*max(BVar$y),
           size = 5
  )+
  annotate("text",
           label = 2.3331313,
           x = al2*min(BVar$x)+(1-al2)*VaR,
           #Posicion Y
           y = al2*max(BVar$y),
           size = 5
  )

# plot(VarGraf)



#Grafico Multiple -----------------
grid.arrange(
  grobs = list(seriegraf1,seriegraf2,densidades),
  widths = c(3, 2),
  layout_matrix = rbind(c(1, 3),
                        c(2, 3))
)

