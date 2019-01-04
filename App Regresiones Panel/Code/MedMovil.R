#Funcion para analisis de Medias Moviles


MedMovBeta = function(x,n=5){
  #Simulacion de Distribucion Empirica de Ruido -------------------------------------
  set.seed(1)
  #Funcion de Distribucion Empirica de los Datos (Xi)   
  DistribEmpirica = function(Lista_Xi,x=Lista_Xi){
    Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
    Distrib = c()
    
    for(i in 1:length(x)){
      Indicatriz = as.numeric(Lista_Xi <= x[i])
      Distrib[i] = mean(Indicatriz, na.rm = TRUE)
    }
    
    return(Distrib)
  }
  #Simulacion a Partir de Distribucion Empirica
  rEmpirica = function(Lista_Xi,n=1){
    Lista_Xi = Lista_Xi[!is.na(Lista_Xi)]
    if(n>0){
      u =runif(n)
      xsim=c()
      for(k in 1:n){
        xsim[k] = min(Lista_Xi[u[k]<=DistribEmpirica(Lista_Xi)],na.rm = TRUE)
      }
      
    }else{
      xsim = NA
      print("Ingrese un n adecuado")
    }
    
    return(xsim)
  }
  
  #Algoritmo Medias Moviles
  mvx = stats::filter(x, rep(1 / n, n), sides = 2)
  resx = x - mvx
  
  resxRecup = resx
  resxRecup[is.na(resxRecup)] = rEmpirica(Lista_Xi = resx, n = sum(is.na(resx)))
  
  mvxRecup = x - resxRecup
  
  # par(mfrow = c(2, 1))
  # 
  # plot(x, main = paste0("IPC, Media Movil [",n,"]"),type = "l", ylab = "IPC")
  # lines(mvxRecup,col= "red")
  # lines(mvx , col = "blue")
  # plot(resxRecup, ylab = "Residuos", main = "Residuos",col="red")
  # lines(resx)
  return(data.frame(x,mvx,resx,mvxRecup,resxRecup))
}

#Media Movil del IPC GENERAL
# mav12Gen = MedMovBeta(x = IPC$GENERAL , n = 12 )
