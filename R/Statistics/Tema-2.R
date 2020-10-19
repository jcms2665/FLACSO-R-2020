#--------------------------------------------------------------------------------
#                            MUESTRAS COMPLEJAS
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Analisis estadístico con muestras complejas (encuestas)

# MATERIAL: Encuesta Nacional de Ocupación y Empleo, primer trimestre
#           de 2020, INEGI.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Tipos de variables
#       5. Esquema de muestreo
#       6. Precisiones estadísticas
#           6.1 Totales
#               6.1.1 Error estándar
#               6.1.2 Coeficiente de variación
#       7. Pruebas de hipótesis
#           7.1 t-Student 
#               7.1.1 Dos colas
#               7.1.2 Una cola
#           7.2 Z o Binomial
#               7.2.1 Dos colas
#           7.3 Z o Binomial de 2 poblaciones
#               7.3.1 Dos colas


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

#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/Desktop/Análisis estadístico/Datos/ENOE_2020")
#---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------


#---------------------------------------
# NOTA:
# ¿Cómo se ven los programas?
# https://github.com/jcms2665/ACP
#---------------------------------------


#3.  Importar datos

sdemt<-data.frame(read.csv("sdEMT120.csv", header = TRUE))
names(sdemt)

sdemt.1<-sdemt%>%rename(r_def=ï..r_def)

#4. Tipos de variables

sdemt.1$r_def <-as.numeric(as.character(sdemt.1$r_def))
sdemt.1$c_res <-as.numeric(as.character(sdemt.1$c_res))
sdemt.1$eda <-as.numeric(as.character(sdemt.1$eda))
sdemt.1$sex <-as.numeric(as.character(sdemt.1$sex))
sdemt.1$clase2 <-as.numeric(as.character(sdemt.1$clase2))
sdemt.1$hrsocup <-as.numeric(as.character(sdemt.1$hrsocup))


#5. Esquema de muestreo

sd<-sdemt.1[which(sdemt.1$eda>=15 & sdemt.1$eda<=98 & sdemt.1$r_def==0 & 
                  (sdemt.1$c_res==1 | sdemt.1$c_res==3)),]

ds_enoe<-svydesign(id=~upm, strata=~est_d, weight=~fac, data=sd, nest=TRUE)
options(survey.lonely.psu="adjust")

#6. Precisiones estadísticas

#6.1 Totales

#6.1.1 Error estándar

# Cálculo de la desviación estandar
ocupacion<-svytotal(~factor(clase2), ds_enoe, deff=TRUE)

# Se guardan los resultados en una tabla
t1<-ocupacion%>%data.frame()


#6.1.2 Coeficiente de variación

# Cálculo del coeficiente de variación
cv<-cv(ocupacion)*100

# Se agrega todo en una sola tabla
t1<-cbind(t1, cv)


#-------------------------------------------
# NOTA:
# Manejo de encuestas (ENOE):
# https://rpubs.com/jcms2665/CS
#-------------------------------------------






#7. Pruebas de hipótesis


#---------------------------------------
# NOTA:
# Las hipótesis son afirmaciones sobre
# un parámetro de la población bajo 
# estudio, no sobre la muestra.
#---------------------------------------


#7.1 t-Student 

#7.1.1 Dos colas

# Supuestos:
# i.  La muestra sigue una distribución normal
# ii. Tamaño de muestra pequeño


#-------------------------------------------
# NOTA:
# Distribución normal:
# https://jcms2665.shinyapps.io/BootstrapS/
#-------------------------------------------
    set.seed(1234)
    
    ocup<-sd[which(sd$sex==1 & sd$eda==18), c("hrsocup")]
    
    
    
    ocup.muestra<-sample(ocup, 15)


#---------------------------------------
# NOTA:
# Se recomienda la lectura del texto:
# The ASA Statement on p-Values: Context, 
# Process, and Purpose
#
#
# Sobre el p-valor:
# i.    No garantiza que H0 sea cierta, 
#       solo define su aceptación o rechazo
# ii.   Rechazar H0 no significa aceptar Ha
# iii.  El p-valor es un recurso matemático,
#       y las decisiones no se deben basar
#       por completo en él.
# iv.   p-valor mide la congruencia de los
#       con aquello que el investigador
#       supone.
#-------------------------------------------

    
    
    
    # H0: media igual a 8
    # Ha: media diferente de 8 
    
    # Criterios:
    
    # p<0.05 rechazamos H0
    # p>0.05 rechazamos Ha
    
    t.test(ocup.muestra, mu=8)
    
    mean(ocup.muestra)
    
    # p-value = 0.3903 > 0.05
    # Aceptamos que la media es 8 horas


    #7.1.2 Una cola
    
    
    ocup.muestra<-sample(ocup, 20)
    
    # H0: media igual a 8
    # Ha: media mayor a 10
    
    # Criterios:
    
    # p<0.05 rechazamos H0
    # p>0.05 rechazamos Ha
    
    t.test(ocup.muestra, mu=10, alternative= "greater")
    
    # p-value = 0.2644 < 0.05
    # Rechazamos que la media es mayor a 8 horas
    
    
    #7.1 Z o Binomial
    
    #7.1.1 Dos colas
    
    # Supuestos:
    # i.  La muestra sigue una distribución Binomial (Exito, fracaso)
    # ii. Tamaño de muestra pequeño
    
    ssocial<-sd[which(sd$clase2==1), c("seg_soc")]
    
    ssocial.muestra<-sample(ssocial, 300)
    
    
    #
    n<-length(ssocial.muestra)
    x<-table(ssocial.muestra)[1]
    
    binom.test(x,n,p=0.30,alternative=c("two.sided"))
    
    # p-value = 1.228e-05 < 0.05
    # Con un 95% de confianza, rechazamos que la proporción 
    # de asegurados es del 30%



#7.3 Z o Binomial de 2 poblaciones

    #7.3.1 Dos colas
    ssocial<-sd[which(sd$clase2==1), c("seg_soc", "sex")]
    
    table(ssocial$sex)
    table(ssocial$seg_soc, ssocial$sex)
    
    n<-c(108219,74750)
    x<-c(46146,31821)  
    prop.test(x,n, correct = TRUE)
    
    # p-value = 0.7651> 0.05
    # Con un 95% de confianza, NO rechazamos que la proporción 
    # de asegurados entre hombres y mujeres es igual


