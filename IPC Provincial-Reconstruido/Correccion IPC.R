#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -------------    Correccion IPC: Con y sin Snto. Domingo   ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(dplyr)
library(readr)

# Carga de Datos  -----------

load("IPC Provincial - Incompleto/Data/IPC_reconstruido.RData")
IPC_incomp = IPC_prov
load("IPC Provincial - Aprox - Completo/Data/IPC_reconstruido.RData")
IPC_comple = IPC_prov
remove(IPC_prov)

# Consolidación de datos IPC_incomple y IPC_comple
IPC_prov_corre = list()
for(i in 1:length(IPC_comple)){
  IPC_prov_corre[[i]] = IPC_incomp[[i]]
  IPC_prov_corre[[i]][1:120,] = IPC_comple[[i]][1:120,]
}
names(IPC_prov_corre) = names(IPC_comple)
# Guardamos  ----------------------------------
save(IPC_prov_corre,file = "IPC_Provincial.RData")

# Guardamos en CSV's   ------------------------
for(i in 1:length(IPC_prov_corre)){
  write.csv(IPC_prov_corre[[i]],
            file = paste0("IPC Provincial/",names(IPC_prov_corre)[i],".csv"))
}




