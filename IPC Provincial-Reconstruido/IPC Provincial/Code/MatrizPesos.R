# Construccion de Indicadores Alfa (Pesos)  ==================================

#Carga Datos
# library(readxl)
# source(file = "Code/CargaData.R",local = TRUE)

# Metrica "d^2" -----
distCuad = function(prov_i , prov_j){
  nombr_i = as.character(prov_i[1,1])
  nombr_j = as.character(prov_j[1,1])
  # Metrica euclidea
  d =  as.numeric(prov_i[1,2]-prov_j[1,2])^2 + as.numeric(prov_i[1,3]-prov_j[1,3])^2
  return(d)
}

# Construccion de Matriz G (indice "gravitacional")  ---------------
# Matriz 23 x 9
G = matrix(NA,nrow = dim(Poblacion)[1],ncol = dim(CiudadProv)[1])
rownames(G) = Poblacion$PROVINCIA

NombProvCiud = inner_join(data.frame(CIUDAD = names(IPC_ciudades)),CiudadProv,by="CIUDAD")
colnames(G) = NombProvCiud$PROVINCIA


A = G #Matriz Estandarizada
for(i in 1:dim(G)[1]){
  
  for(j in 1:dim(G)[2]){
    pobla_i = Poblacion$POBLACION[i]
    pobla_j = Poblacion %>% 
      filter(PROVINCIA == NombProvCiud$PROVINCIA[j]) %>%
      select(POBLACION) %>% as.numeric()
    j_aux = Poblacion$PROVINCIA == NombProvCiud$PROVINCIA[j]
    if(distCuad(Centroides[i,],Centroides[j_aux,]) != 0){
      G[i,j] = as.numeric(pobla_i * pobla_j)/distCuad(Centroides[i,],Centroides[j_aux,])
    }else{
      G[i,j] = NA
    }
    
  }
  
}
#Correccion d(j_p,j_p)= 0 entonces debemos definir g(j_p, j_p) = 1  y g(j_p, j_q) = 0
BoolG = is.na(G)
for(j in 1:dim(G)[2]){
  G[BoolG[,j],-j] = 0
  G[BoolG[,j],j] = 1
}

# Matriz estandarizada A = (alfa_(i,j)), unitaria por filas ------------
for(i in 1:dim(A)[1]){
  A[i,] = G[i,]/sum(G[i,])
}

remove(i,j,j_aux,G,pobla_i,pobla_j,BoolG)
