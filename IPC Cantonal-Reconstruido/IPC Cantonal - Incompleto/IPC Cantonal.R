#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#-------------   Reconstruccion IPC Cantonal  2019    ---------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Carga Datos
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)

source(file = "Code/CargaData.R",local = TRUE)
source(file = "Code/MatrizPesos.R",local = TRUE)

IPC_canton = list()
for(Ciudd in 1:dim(Poblacion)[1]){
  
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
      for(ciuP in 1:dim(CiudadCanton)[1] ){
        # Sumar IPC de ciudades Principales (Que existen)
        if(!is.na(IPC_ciudades[[ciuP]][i,j])){
          Bd[i,j] = Bd[i,j] + A[Ciudd,ciuP]*IPC_ciudades[[ciuP]][i,j]
        }
        #Si no no suma nada (Ej. Santo Domingo)
        
      }
      if(Bd[i,j] == 0){
        Bd[i,j] = NA
      }
      
    }
    
  }
  IPC_canton[[Ciudd]] = Bd
  print(paste("************ ",paste0(Ciudd, ".- " ,Poblacion$CANTON[Ciudd])," *************"))
}

names(IPC_canton) = paste(Poblacion$COD_CANTON,Poblacion$CANTON)



# ---------------------------------------------
save(IPC_canton,file = "Data/IPC_Canton_reconstruido.RData")
# ---------------------------------------------

# Guardamos en CSV's   ------------------------
for(i in 1:length(IPC_canton)){
  write.csv(IPC_canton[[i]],
            file = paste0("Data/Cantones Reconstruidos/",names(IPC_canton)[i],".csv"))
}


#Data para empatar canton con su Archivo.csv Respectivo  ----------------
ArchCodCanton = data.frame(COD_CANTON = paste0(Poblacion$COD_CANTON),
                       CANTON = Poblacion$CANTON,
                       ARCH_CANTON = paste(Poblacion$COD_CANTON,Poblacion$CANTON))

write.csv(ArchCodCanton, file = "Data/Mapa/ArchCodCanton.csv",row.names = F)

