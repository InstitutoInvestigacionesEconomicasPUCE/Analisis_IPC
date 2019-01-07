# Función para rellenar Medias Moviles con Polinomios Interpolantes

# library(polynom)

# y= c(NA,NA,4,6,9,10,8,5,4,1,NA,NA)
extrapolacion = function(y,ord = 2){
  x= 1:length(y)
  tmin = min(which(!is.na(y)))
  tmax = max(which(!is.na(y)))
  #orden del Polinomio Interpolante
  if(length(y[!is.na(y)])>ord){
    ord = ord+1
    Pinf = as.function(poly.calc(x[tmin:(tmin+ord)],y[tmin:(tmin+ord)]))
    Psup = as.function(poly.calc(x[(tmax-ord):tmax],y[(tmax-ord):tmax]))
  }
  
  #Valores Extrapolados
  y[1:(tmin-1)] = Pinf(1:(tmin-1))
  y[(tmax+1):length(y)] = Psup((tmax+1):length(y))
  return(y)
}

# extrapolacion(y)
# 
# par(mfrow=c(2,1))
# plot(y,type="l",ylim = c(-27,12))
# plot(extrapolacion(y,ord=2),type="l")



