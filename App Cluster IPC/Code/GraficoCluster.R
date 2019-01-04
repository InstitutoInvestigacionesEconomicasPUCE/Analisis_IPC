# Gráfico del IPC por Producto(Item)  ---------------------
graf_series = function(IPC,items,ind,metric,cluster){
  
  aux = IPC[,items[ind]]
  
  graf=dygraph(aux, main = paste("IPC (Base 2004):","Cluster",cluster),group = 'itemsG')%>%
    dyAxis("x", label=names(metric)) %>%
    dyAxis('y',label='IPC (Base 2004)')%>%
    dyHighlight(highlightSeriesBackgroundAlpha = 0.3)%>%
    dyLegend(show= "follow",width = 400)
  return(graf)
}

graf_clus = function(IPC,cl,metricasList){
  
  # # Gráfico del IPC por Producto(Item)  ---------------------
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
  
  # Grafico de Series por Cluster  ------------------------
  resultado = list()
  for(i in 1:N){
    resultado[[i]] = graf_series(IPC,items,cl[[i]],metricasList[m])
  }
  return(resultado)
}



