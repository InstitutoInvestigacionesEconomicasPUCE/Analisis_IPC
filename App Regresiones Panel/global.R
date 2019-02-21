#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

# Analisis de : IPC (Serie Empalmada Regional)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------        ANALISIS Regresion IPC - PUCE      ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
library(latex2exp)
#Tablas
library(readxl)
library(readr)
library(curl)
library(xtable)
library(DT)
library(tidyverse)
#Series de Tiempo
library(polynom)
library(TSdist)
library(ggplot2)
library(gridExtra)
# library(xts)
library(forecast)
library(hexbin)
library(MASS)
#>> Carga de Datos -----------------------------------------------
# http://www.ecuadorencifras.gob.ec/series-empalmadas-ipc-base-2004/
# IPC = read_csv("Data/IPChistorico.csv")

source(file ="Code/CargaData.R" ,local = TRUE)

productos = names(IPC)[-1]
ProductosLista = 1:length(productos)
names(ProductosLista) = paste(ProductosLista,".",productos)
# IPC = data.frame(IPC)
# names(IPC) = c("Fecha", productos)

PeriodoLista = 2006:2018
names(PeriodoLista) = paste("Año:",PeriodoLista)

#Tipo de Estandarizacion (Deflactar)
TipoDeflactor = c("MM12(IPC General)" = 1, "MM12(Serie Original)" = 2)

#-------------------------------------------
source(file ="Code/MedMovil.R" ,local = TRUE)
#------------------------------------------------
