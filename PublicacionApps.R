#=================    PUBLICACION EN SHINY SERVER    =======================
getwd()
# Aplicacion de TS Cluster IPC --------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App Cluster IPC",
                     account = "cristianpachacama",
                     appName = "ClusterIPC")

# Aplicacion de Regresion Log IPC --------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App Regresion IPC",
                     account = "cristianpachacama",
                     appName = "RegresionIPC")

# Aplicacion Distancia K-S Distribucion --------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("Distancias Distribuciones IPC",
                     account = "cristianpachacama",
                     appName = "DistribucionKS_IPC")

# Versiones Noviembre 2018 -----------------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App Regresiones Panel",
                     account = "cristianpachacama",
                     appName = "Analisis_IPC_Ecuador")

# Versiones Enero 2019 -----------------
rsconnect::setAccountInfo(name='iiepuce',
                          token='53037FA556A5F1E9C83C39DB300CFD72',
                          secret='DxF1zddv2UzfBFJi15z3US7eLs+bhnC/qv0QPA4M')

rsconnect::deployApp("App Regresiones Panel",
                     account = "iiepuce",
                     appName = "Analisis_IPC_Ecuador")


