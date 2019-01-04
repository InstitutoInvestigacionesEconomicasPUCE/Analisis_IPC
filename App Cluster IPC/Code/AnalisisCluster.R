

EstudioIPC = function(IPCx,metricasList,metrica=17,clusters=7,
                      p=5,window.size=5,g=2,sigma=2,
                      epsilon=0.5,tau=2.5,w=3,h=4){
  
  # Items = IPChist$ITEM
  # items = as.vector(abbreviate(IPChist$ITEM))
  # colnames(IPC) = items
  # IPC = ts(IPC, start = c(2005, 1),frequency = 12)
  # dim(IPC)
  
  # Gráfico del IPC por Producto(Item)  ---------------------
  # graf_series = function(IPC,items,ind,metric){
  #   
  #   aux = IPC[,items[ind]]
  #   
  #   dygraph(aux, main = "IPC (Base 2004)",group = 'itemsG')%>%
  #     dyAxis("x", label=metric) %>%
  #     dyAxis('y',label='IPC (Base 2004)')%>%
  #     dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
  #     dyLegend(show= "follow",width = 400)
  #   
  # }
  
  # Matriz de Distancia -----------------
  # metricasList = c("euclidean", "manhattan", "minkowski", 
  #                  "infnorm", "ccor", "sts", "dtw", "lb.keogh", 
  #                  "edr", "erp", "lcss", "fourier", "tquest", 
  #                  "dissim", "acf", "pacf", "ar.lpc.ceps", 
  #                  "ar.mah", "ar.mah.statistic", "ar.mah.pvalue", 
  #                  "ar.pic", "cdm", "cid", "cor", "cort", "wav", 
  #                  "int.per", "per", "mindist.sax", "ncd", "pred", 
  #                  "spec.glk", "spec.isd", "spec.llr", "pdc", "frechet")
  matriz_dist = function(IPC,metrica=metricasList[1]){
    
    D = matrix(data=rep(NA,116^2),nrow=116,ncol = 116)
    for(i in 1:116){
      for(j in 1:116){
        if(i<j){
          if(i==3){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,p)
          }
          if(i==8){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,window.size)
          }
          if(i==10){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,g,sigma)
          }
          if(i==11){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,epsilon,sigma)
          }
          if(i==13){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,tau)
          }
          if(i==29){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,w)
          }
          if(i==31){
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica,h)
          }else{
            D[i,j]=TSDistances(IPC[,i],IPC[,j],distance = metrica)
          }
          
          D[j,i]=D[i,j]
        }
      }
      D[i,i]=0
      
    }
    return(D)
  }
  #Metrica de Atocorrelacion: 15,16
  #Metricas ARMA: 17 - 21
  m=metrica
  D=matriz_dist(IPCx,metricasList[m])
  
  #nube = smacofSym(D)
  #plot(nube)
  
  #Clusterizacion CLARA ----------------------
  Clustering = function(D,N=5){
    
    clus.fit = clara(D, k=N, samples = 50, stand=TRUE, pamLike = TRUE)
    #plot(clus.fit)
    cl =list()
    for(i in 1:N){
      cl[[i]] = which(clus.fit$clustering==i)
    }
    return(cl)
  }
  
  N = clusters
  cl = Clustering(D,N)
  
  # Grafico de Series  ------------------------
  # resultado = list()
  # for(i in 1:N){
  #   resultado[[i]] = graf_series(IPC,items,cl[[i]],metricasList[m])
  # }
  
  return(cl)
}



