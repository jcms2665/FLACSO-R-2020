#--------------------------------------------------------------------------------
#                    MANEJO DE BASES DE DATOS - REPASO
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Analizar la ECOVID-ML con R

# MATERIAL: Encuesta Telefónica sobre COVID-19 y Mercado Laboral, INEGI.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías

#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo


rm(list=ls())                 # 
graphics.off()                # 
options(warn=-1)              # Evita que aparezcan warnings


#1. Cargar librerías

library(foreign)                 
library(questionr)              
library(ggplot2) 
library(dplyr) 

#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/30. FLACSO/Análisis estadístico/Datos/F00008548-Latinobarometro_2018_Esp_R_v20190303")


latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")
table(latino$IDENPA)
table(latino$P10STC.A)
wtd.table(latino$IDENPA, weights =latino$WT)
table(latino$WT)
