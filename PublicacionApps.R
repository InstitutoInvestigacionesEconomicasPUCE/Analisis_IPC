#=================    PUBLICACION EN SHINY SERVER    =======================
getwd()

# Versiones Noviembre 2018 -----------------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxXXXXXXXXXXXXXXXXXXXXXXXXXXX5IG')

rsconnect::deployApp("App Regresiones Panel",
                     account = "cristianpachacama",
                     appName = "Analisis_IPC_Ecuador")

# Versiones Enero 2019 -----------------
rsconnect::setAccountInfo(name='iiepuce',
                          token='53037FA556A5F1E9C83C39DB300CFD72',
                          secret='DxF1zddvXXXXXXXXXXXXXXXXXXXXXXXqv0QPA4M')

rsconnect::deployApp("App Regresiones Panel",
                     account = "iiepuce",
                     appName = "Analisis_IPC_Ecuador")


