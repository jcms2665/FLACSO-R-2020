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
#           7.4 Chi-cuadrada


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

sdemt<-data.frame(read.csv("SDEMT120.csv", header = TRUE))
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
ocupacion<-svytotal(~factor(pos_ocu), ds_enoe, deff=TRUE)

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

# El nivel de significancia α determina la probabilidad 
# de error que se quiere asumir a la hora de rechazar la 
# hipótesis nula. Se emplea como punto de referencia para 
# determinar si el valor de p-value obtenido en el test de 
# hipótesis es suficientemente bajo como para considerar 
# significativas las diferencias observadas y por lo tanto 
# rechazar H0. 

# p<0.05 rechazamos H0
# p>0.05 rechazamos Ha

t.test(ocup.muestra, mu=8)

# p-value = 0.3903 > 0.05
# Aceptamos que la media es 8 horas

mean(ocup.muestra)

# Con un 95% de confianza, aceptamos que los hombres de 18 años
# trabajan un promedio de 8 horas a la semana


#---------------------------------------------
# RETO-1:
# ¿Es verdad que las mujeres de 24 años
# trabajan 8 horas a la semana?
# i.    Plantea las hipótesis
# ii.   Realiza la prueba correspondiente
#       con una muestra de 17 casos
# iii.  Interpreta los resultados
#---------------------------------------------



mean(ocup.muestra)



#7.1.2 Una cola


ocup<-sd[which(sd$sex==1 & sd$eda==18), c("hrsocup")]

ocup.muestra<-sample(ocup, 20)

# H0: media igual a 10
# Ha: media mayor a 10


t.test(ocup.muestra, mu=10, alternative= "greater")

# p-value = 0.7246 > 0.05
# Rechazamos que la media es mayor a 8 horas


#---------------------------------------------
# RETO-2:
# Aumenta el tamaño de muestra a 150
# ¿Qué ocurre?
#---------------------------------------------



#7.1 Z o Binomial

#7.1.1 Dos colas

# Supuestos:
# i.  La muestra sigue una distribución Binomial (Exito, fracaso)
# ii. Tamaño de muestra pequeño


ssocial<-sd[which(sd$clase2==1), c("seg_soc")]

ssocial.muestra<-sample(ssocial, 300)


#
n<-300
table(ssocial.muestra)
x<-129

razon<-x/n
razon

binom.test(x,n,p=0.30,alternative=c("two.sided"))

# p-value = 1.228e-05 < 0.05
# Con un 95% de confianza, rechazamos que la proporción 
# de asegurados es del 30%


#---------------------------------------------
# RETO-3:
# i.    Etiqueta la variable
# ii.   ¿Es verdad que la proporción de los
#       no asegurados es de 30%?
# iii.  Interpreta los resultados
#---------------------------------------------




#7.3 Z o Binomial de 2 poblaciones

#7.3.1 Dos colas
ssocial<-sd[which(sd$clase2==1), c("seg_soc", "sex")]

table(ssocial$sex)
table(ssocial$seg_soc, ssocial$sex)

n<-c(108219,74750)
x<-c(46146,31821)  
prop.test(x,n, correct = TRUE)

# p-value = 0.7651> 0.05
# Con un 95% de confianza, aceptamos que la proporción 
# de asegurados entre hombres y mujeres es igual

#---------------------------------------------------
# RETO-4:
# ¿Qué sucede con los adultos mayores (60 +), la
# proporción de asegurados es igual?
#---------------------------------------------------


#8. Chi-cuadrada

# Las hipótesis que se prueban en una ji cuadrada son :
#     H0: las dos variables son independientes
#     HA: las dos variables no son independientes

#     p ≤ α: Las variables tienen una asociación 
#     estadísticamente significativa (Rechazar H0)

#     Valor p > α: No se puede concluir que las 
#     variables están asociadas (No se puede rechazar H0)

trab<-filter(sdemt.1, clase2==1)

trab.t<-wtd.table(trab$pos_ocu, trab$sex)

chisq.test(trab.t) 

# p-value = 2.2e-16< 0.05

#---------------------------------------------------
# RETO-5:
# Tener seguridad social, ¿depende del sexo de 
# las personas?
#---------------------------------------------------

trab<-filter(sdemt.1, clase2==1)

trab.t<-wtd.table(trab$seg_soc, trab$sex)

chisq.test(trab.t) 

