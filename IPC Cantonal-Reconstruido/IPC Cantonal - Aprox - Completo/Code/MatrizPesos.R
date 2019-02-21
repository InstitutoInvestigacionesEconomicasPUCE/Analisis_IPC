# Construccion de Indicadores Alfa (Pesos)  ==================================

#Carga Datos
# library(readxl)
# source(file = "Code/CargaData.R",local = TRUE)

# Metrica "d^2" -----
distCuad = function(prov_i , prov_j){
  nombr_i = as.character(prov_i$CANTON[1])
  nombr_j = as.character(prov_j$CANTON[1])
  # Metrica euclidea
  d =  as.numeric(prov_i$POINT_X[1]-prov_j$POINT_X[1])^2 + as.numeric(prov_i$POINT_Y[1]-prov_j$POINT_Y[1])^2
  return(d)
}

# Construccion de Matriz G (indice "gravitacional")  ---------------
# Matriz 23 x 9
G = matrix(NA,nrow = dim(Poblacion)[1],ncol = dim(CiudadCanton)[1])
rownames(G) = Poblacion$CANTON

NombCantCiud = inner_join(data.frame(CIUDAD = names(IPC_ciudades)),CiudadCanton,by="CIUDAD")
colnames(G) = NombCantCiud$CANTON

A = G #Matriz Estandarizada
for(i in 1:dim(G)[1]){
  
  for(j in 1:dim(G)[2]){
    pobla_i = Poblacion$POBLACION[i]
    pobla_j = Poblacion %>% 
      filter(CANTON == NombCantCiud$CANTON[j]) %>%
      select(POBLACION) %>% as.numeric()
    j_aux = Poblacion$COD_CANTON == NombCantCiud$COD_CANTON[j]
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
