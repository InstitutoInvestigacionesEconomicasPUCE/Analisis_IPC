#------------------------------------------------------------
#!!!!!!!!!!!!!!!!!!!   Regresion Simple IPC    !!!!!!!!!!!!!!
#------------------------------------------------------------

#Producto
 k = as.numeric(input$producto)
# k=1
# IPC = read_csv("Data/IPChistoricoTrn.csv")

# productos = names(IPC)[-1]
# ProductosLista = 1:length(productos)
# names(ProductosLista) = productos
# IPC = data.frame(IPC)
# names(IPC) = c("Fecha", productos)
Fecha = as.Date(IPC$Fecha, format = "%d-%m-%y")
BDDgraf = data.frame(Fecha, SerieOrig =  IPC[,k+1])
#Datos IPC producto k
IPCp=IPC[,k+1]

#Modelo

# confint.lm(modelo1)
temp=1:length(IPCp)
modelo1=lm(IPCp~temp)
# summary(modelo1)
#IPCfit = forecast(modelo)
#Quitado del UI la opcion de eleccion del deflactor!!

