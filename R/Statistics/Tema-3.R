#--------------------------------------------------------------------------------
#                            MUESTRAS COMPLEJAS
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Introducción al análisis multivariado

# MATERIAL: Encuesta Nacional de Ocupación y Empleo, primer trimestre
#           de 2020, INEGI.

#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Análisis de conglomerados
#           4.1 Normalizar datos
#           4.2 K-medias
#           4.3 ¿Quiénes están en los grupos?
#       5. Análisis de componentes principales
#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo


rm(list=ls())                 # 
graphics.off()                # 
options(warn=-1)              # Evita que aparezcan warnings


#1. Cargar librerías

library(foreign)                 
library(questionr)              
library(survey) 
library(car)
library(tidyverse)
library(stats)

#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/Desktop/Análisis estadístico/Datos/ENOE_2020")


#3.  Importar datos

sdemt<-data.frame(read.csv("SDEMT120.csv", header = TRUE))


#4. Análisis de conglomerados

# El objetivo de esta técnica es hacer grupos homogéneos al interior
# y heterogéneos al exterior

ocupados<-filter(sdemt, clase2==1)
ac.base<-filter(sdemt, clase2==1)%>% select(anios_esc,hrsocup, ingocup)

# Validar tipo de datos

class(ac.base$anios_esc)
class(ac.base$hrsocup)
class(ac.base$ingocup)

ac.base$anios_esc<-as.numeric(ac.base$anios_esc)
ac.base$hrsocup<-as.numeric(ac.base$hrsocup)
ac.base$ingocup<-as.numeric(ac.base$ingocup)

#4.1 Normalizar datos

ac.base<-mutate(ac.base, es.nor=(anios_esc-min(anios_esc))/(max(anios_esc)-min(anios_esc)))
ac.base<-mutate(ac.base, hr.nor=(anios_esc-min(hrsocup))/(max(hrsocup)-min(hrsocup)))
ac.base<-mutate(ac.base, ing.nor=(anios_esc-min(ingocup))/(max(ingocup)-min(ingocup)))

ac.base.nor<-ac.base[, 4:6]

#4.2 K-medias

fit <- kmeans(ac.base.nor, 3)

ocupados <- data.frame(ocupados, fit$cluster)

table(ocupados$fit.cluster)

#4.3 ¿Quiénes están en los grupos?

# Etiquetar las variables cs_p13_1 y e_con

table(ocupados$cs_p13_1, ocupados$fit.cluster)
table(ocupados$e_con, ocupados$fit.cluster)




#5. Análisis de componentes principales


mujeres<-filter(sdemt, sex==2)
acp.base<-mujeres%>% select(anios_esc,hrsocup, ingocup, n_hij, eda)

# Validar tipo de datos

class(acp.base$anios_esc)
class(acp.base$hrsocup)
class(acp.base$ingocup)
class(acp.base$n_hij)
class(acp.base$eda)

acp.base$anios_esc<-as.numeric(acp.base$anios_esc)
acp.base$hrsocup<-as.numeric(acp.base$hrsocup)
acp.base$ingocup<-as.numeric(acp.base$ingocup)
acp.base$n_hij<-as.numeric(acp.base$n_hij)
acp.base$eda<-as.numeric(acp.base$eda)

Acomp<-prcomp(na.omit(acp.base), scale=TRUE)

summary(Acomp)

plot(Acomp,type="lines")

Acomp$rotation%*%diag(Acomp$sdev)

