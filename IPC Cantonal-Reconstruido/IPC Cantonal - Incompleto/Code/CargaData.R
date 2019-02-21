# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------   Carga de Datos   ---------------------
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Carga de Datos de Población y Ubicacion  =================
Poblacion = read_excel("Data/POBLACION_CANTONES.xlsx")
Centroides = read_excel("Data/CENTROIDE_CANTONES.xlsx")
CiudadCanton = read_excel("Data/CANTON_CIUDAD.xlsx")

# Archivo IPC (Serie Empalmada Regional)
# http://www.ecuadorencifras.gob.ec/series-empalmadas-ipc-base-2004/


# Limpieza de Datos   ======================================

# IPC_original = read_excel("Data/ipc_ind_nac_reg_ciud_emp_clase_12_2018.xls",
#                           skip=4,sheet = 2,n_max = 116)
LimpiezaIPC = function(IPC_original){
  
  IPC_aux = as.data.frame( IPC_original[,-(1:2)])
  IPC_dat = t(IPC_aux[,-1])
  #Vector de Fechas
  fechas = as.Date("1899-12-30") + as.numeric(names(IPC_aux)[-1])
  # fechas = as.character(format(fechas, "%d-%m-%y"))
  fechas = as.character(format(fechas, "%Y-%m-%d"))
  
  IPC = data.frame(Fecha=fechas, IPC_dat,stringsAsFactors = FALSE )
  names(IPC) = c("Fecha",IPC_aux[,1])
  rownames(IPC) = 1:(dim(IPC)[1])
  
  remove(IPC_aux,IPC_dat)
  
  return(IPC)
}

#Cargar Datos de 9 Ciudades  =======================================
IPC_ciudades = list()
for(p in 5:13){
  aux = read_excel("Data/IPC/ipc_ind_nac_reg_ciud_emp_clase_12_2018.xls",
                   skip=4,sheet = p,n_max = 116)
  IPC_ciudades[[p-4]] = LimpiezaIPC(aux)
}
remove(aux)
names(IPC_ciudades) = c("GUAYAQUIL","ESMERALDAS","MACHALA","MANTA","SANTO DOMINGO",
                        "QUITO","LOJA","CUENCA","AMBATO")



# Desde la Web con el URL -----
# url = "http://www.ecuadorencifras.gob.ec/documentos/web-inec/Inflacion/2018/Noviembre-2018/Historicos_completo/SERIES%20IPC%20EMPALMADAS/ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls"
# destfile = "ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls"
# curl::curl_download(url, destfile)
# IPC_original = read_excel(destfile, skip=4,sheet = 2,n_max = 116)

