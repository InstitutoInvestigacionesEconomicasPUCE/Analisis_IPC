#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------   Reconstruccion IPC Provincial 2019    ---------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Carga Datos
library(readxl)
library(ggplot2)
library(dplyr)

source(file = "Code/CargaData.R",local = TRUE)
source(file = "Code/MatrizPesos.R",local = TRUE)

IPC_prov = list()
for(Prov in 1:dim(Poblacion)[1]){
  
  #Data Frames Inicializados   -----------------
  nfila = dim(IPC_ciudades[[1]])[1]
  ncolum = dim(IPC_ciudades[[1]])[2]
  Bd = matrix(NA,nrow = nfila, ncol = ncolum)
  Bd = data.frame(Bd)
  names(Bd) = names(IPC_ciudades[[1]])
  Bd$Fecha = IPC_ciudades[[1]]$Fecha
  #Rellenamos con IPC Reconstruido  ------------
  for(j in 2:ncolum){
    
    for(i in 1:nfila){
      Bd[i,j] = 0
      for(ciuP in 1:dim(CiudadProv)[1] ){
        # Sumar IPC de ciudades Principales (Que existen)
        if(!is.na(IPC_ciudades[[ciuP]][i,j])){
          Bd[i,j] = Bd[i,j] + A[Prov,ciuP]*IPC_ciudades[[ciuP]][i,j]
        }
        #Si no no suma nada (Ej. Santo Domingo)
        
      }
      if(Bd[i,j] == 0){
        Bd[i,j] = NA
      }
      
    }
    
  }
  IPC_prov[[Prov]] = Bd
  print(paste0("********   IPC-Provincia ",Prov," :",
               Poblacion$PROVINCIA[Prov],
               "   *********"))
}

names(IPC_prov) = Poblacion$PROVINCIA


# ---------------------------------------------
save(IPC_prov,file = "Data/IPC_reconstruido.RData")
# ---------------------------------------------



# Guardamos en CSV's   -----------------------
for(i in 1:length(IPC_prov)){
  write.csv(IPC_prov[[i]],
            file = paste0("Data/Provincias Reconstruidas/",names(IPC_prov)[i],".csv"))
}

#---------------------------------------------



#Grafico Ejemplo   -----------------
# aux = IPC_prov[[1]]
# 
# BDDgraf = data.frame(Fecha= as.Date(aux$Fecha,format = "%d-%m-%y"), 
#                      SerieOrig =  aux$GENERAL)
# 
# ggplot(data = BDDgraf, aes(x = Fecha, y = SerieOrig)) +
#   geom_line(size = 0.7,colour = "black")






