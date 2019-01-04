# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------   Carga de Datos   ---------------------
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# Archivo IPC (Serie Empalmada Regional)
# http://www.ecuadorencifras.gob.ec/series-empalmadas-ipc-base-2004/


# Desde la Web con el URL ----------
# url = "http://www.ecuadorencifras.gob.ec/documentos/web-inec/Inflacion/2018/Noviembre-2018/Historicos_completo/SERIES%20IPC%20EMPALMADAS/ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls"
# destfile = "ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls"
# curl::curl_download(url, destfile)
# IPC_original = read_excel(destfile, skip=4,sheet = 2,n_max = 116)


# Carga Datos IPC Nacional   -----------
IPC_original = read_excel("Data/ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls",
                          skip=4,sheet = 2,n_max = 116)
# Estandarización  -----------------
IPC_aux = as.data.frame( IPC_original[,-(1:2)])
IPC_dat = t(IPC_aux[,-1])

fechas = as.Date("1899-12-30") + as.numeric(names(IPC_aux)[-1])
fechas = as.character(format(fechas, "%d-%m-%y"))

IPC = data.frame(Fecha=fechas, IPC_dat)
names(IPC) = c("Fecha",IPC_aux[,1])
rownames(IPC) = 1:(dim(IPC)[1])

remove(IPC_aux,IPC_dat)




