# Carga Reactiva 

IPCreact = reactive({
  
  # IPC Segun Region  -----------
  IPC_original = read_excel("Data/ipc_ind_nac_reg_ciud_emp_clase_11_2018.xls",
                            skip=4,sheet = as.numeric(input$region)+1, n_max = 116)
  # Estandarización  -----------------
  IPC_aux = as.data.frame( IPC_original[,-(1:2)])
  IPC_dat = t(IPC_aux[,-1])
  
  fechas = as.Date("1899-12-30") + as.numeric(names(IPC_aux)[-1])
  fechas = as.character(format(fechas, "%d-%m-%y"))
  
  IPC = data.frame(Fecha=fechas, IPC_dat)
  names(IPC) = c("Fecha",IPC_aux[,1])
  rownames(IPC) = 1:(dim(IPC)[1])
  
  # remove(IPC_aux,IPC_dat)
  
  return(IPC)
  
})