#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -------------    Correccion IPC: Con y sin Snto. Domingo   ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(dplyr)
library(readr)

# Carga de Datos  -----------

load("IPC Cantonal - Incompleto/Data/IPC_Canton_reconstruido.RData")
IPC_incomp = IPC_canton
load("IPC Cantonal - Aprox - Completo/Data/IPC_Canton_reconstruido.RData")
IPC_comple = IPC_canton
remove(IPC_canton)

# Consolidación de datos IPC_incomple y IPC_comple
IPC_canton_corre = list()
for(i in 1:length(IPC_comple)){
  IPC_canton_corre[[i]] = IPC_incomp[[i]]
  IPC_canton_corre[[i]][1:120,] = IPC_comple[[i]][1:120,]
}
names(IPC_canton_corre) = names(IPC_comple)
# Guardamos  ----------------------------------
save(IPC_canton_corre,file = "IPC_Cantonal.RData")

# Guardamos en CSV's   ------------------------
for(i in 1:length(IPC_canton_corre)){
  write.csv(IPC_canton_corre[[i]],
            file = paste0("IPC Cantonal/",names(IPC_canton_corre)[i],".csv"))
}




