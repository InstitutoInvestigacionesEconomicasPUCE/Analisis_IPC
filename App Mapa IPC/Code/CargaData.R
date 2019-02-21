
# DATOS PROVINCIAL  ========================

# DESDE ARCHIVOS CSV    --------------------
# list_prov = read_excel("Data/PROVINCIAL/PROVINCIAS.xlsx")
# list_prov = list_prov$PROVINCIA
# IPC_prov = list()
# for(i in seq(list_prov)){
#   aux = read_csv(paste0("Data/PROVINCIAL/Provincias/",list_prov[i],".csv"),
#                  locale = locale(encoding = "ISO-8859-1"))
#   nomb_aux = names(aux)
#   # aux$Fecha = as.Date(aux$Fecha,format=)
#   aux = data.frame(aux)
#   names(aux) = nomb_aux
#   IPC_prov[[i]] = aux[,-1]
# }
# names(IPC_prov) = list_prov
# remove(aux,i,list_prov)


load("Data/PROVINCIAL/IPC_Provincial.RData")
IPC_prov = IPC_prov_corre
remove(IPC_prov_corre)
# Mapa Ecuador JSON  ------------------------
MapaProv = fromJSON(file= "Data/PROVINCIAL/Mapa/Mapa_Ecuador.json")
PROVprov = read_excel("Data/PROVINCIAL/Mapa/PROV_prov.xlsx")



# DATOS CANTONAL  ===========================
load("Data/CANTONAL/IPC_Cantonal.RData")
IPC_canton = IPC_canton_corre
remove(IPC_canton_corre)

# Mapa Ecuador geoJSON  ---------------------
MapaCanton = fromJSON(file = "Data/CANTONAL/Mapa/Mapa_Cantonal.geojson")
ArchCodCanton = read.csv("Data/CANTONAL/Mapa/ArchCodCanton.csv")
ArchCodCanton$COD_CANTON = substr(ArchCodCanton$ARCH_CANTON,start = 1,stop = 4)


