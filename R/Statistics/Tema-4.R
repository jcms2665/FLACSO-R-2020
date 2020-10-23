#--------------------------------------------------------------------------------
#                           MODELOS ESTADÍSTICOS
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
library(stats) 

#2. Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/Desktop/Análisis estadístico/Datos/Latinobarometro_2018/")


#3. Cargar base de datos

latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")


#4. Regresión lineal

# Pregunta de investigación:
# ¿Cuáles son los factores que influyen en la 
# ideología política?

#4.1 Variable dependiente/objetivo


# Ideología: Escala Izquierda-Derecha (P22ST)
# 0=Izquierda, 10 Derecha


class(latino$P22ST)
latino$P22ST<-as.numeric(latino$P22ST)
class(latino$P22ST)

table(latino$P22ST)


l.1<-filter(latino,P22ST>=0)
table(l.1$P22ST)


#4.2 Variables independientes/covariables

# Nivel socioeconómico (P10STC.A)
# 0-Pobre, 10-Rico
table(l.1$P10STC.A)

# Edad de término de estudios (S9)
table(l.1$S9)

# Creamos una nueva base con casos válidos
latino$P10STC.A<-as.numeric(latino$P10STC.A)
latino$S9<-as.numeric(latino$S9)
l.1<-filter(l.1,P10STC.A>=0 & S9>=0)


l.reg<-l.1%>%select(P22ST,P10STC.A, S9)

l.reg<-l.reg%>%rename(ideol=P22ST, pob=P10STC.A, educ=S9)

#4.3 Normalización

l.reg<-mutate(l.reg, ideol.nor=(ideol-min(ideol))/(max(ideol)-min(ideol)))
l.reg<-mutate(l.reg, pob.nor=(pob-min(pob))/(max(pob)-min(pob)))
l.reg<-mutate(l.reg, educ.nor=(educ-min(educ))/(max(educ)-min(educ)))

l.reg.nor<-l.reg[, 4:6]

cor(l.reg.nor)

#4.4 Ajuste del modelo
regresion <- lm(ideol.nor ~ pob.nor+educ.nor, data = l.reg.nor)
summary(regresion)


#4.5 Supuestos

#4.5.1 Homocedasticidad

# Homocedasticidad 
# la varianza del error condicional a las variables explicativas es 
# constante a lo largo de las observaciones

residuos <- rstandard(regresion)
valores.ajustados <- fitted(regresion)
plot(valores.ajustados, residuos)

# Sí se observa un patrón, por lo que el supuesto de homocedasticidad 
# puede no cumplirse

#4.5.2 Normalidad de residuos
 
qqnorm(residuos)
qqline(residuos)
# Los puntos están bastante alineados, 
# la normalidad parece aceptable.


#-------------------------------------------
# RETO-1:
# Ajusta un modelo de regresión para
# algún país (IDENPA):
# IDENPA  32  Argentina
# IDENPA  76  Brasil
# IDENPA  724 España
# IDENPA  858 Uruguay
#-------------------------------------------





#5. Regresión logística

latino <- readRDS("Latinobarometro_2018_Esp_R_v20190303.Rds")

# Pregunta de investigación:
# ¿Cuáles son los factores que influyen para que 
# las personas estén satisfechas -o no-?

#5.1 Variable dependiente/objetivo

# Grado de satisfacción con la vida (P1STC)
table(latino$P13STGBS.A)
# 1.- Muy satisfecho
# 2.- Bastante satisfecho
# 3.- No muy satisfecho
# 4.- Para nada satisfecho
# -1-.- No answer/Refused
# -4-.- No preguntada

class(latino$P13STGBS.A)
latino$P13STGBS.A<-as.numeric(as.character(latino$P13STGBS.A))
class(latino$P13STGBS.A)

s1<-filter(latino,P13STGBS.A>=0)
table(s1$P13STGBS.A)

s1$satisfecho[s1$P13STGBS.A==1 |s1$P13STGBS.A==2]<-1
s1$satisfecho[s1$P13STGBS.A==3 |s1$P13STGBS.A==4]<-0

table(s1$satisfecho)


#5.2 Variables independientes/covariables

# Confianza en la Iglesia (P15STGBSC.C)
table(s1$P15STGBSC.C)


# Aprueba al presidente (P20STGBSC)
table(s1$P20STGBSC)

# SEXO
table(s1$SEXO)

# EDAD
table(s1$EDAD)

# Renombrar variables
s1<-s1%>%rename(igl=P15STGBSC.C, pres=P20STGBSC, sexo=SEXO)
s1<-s1%>%select(satisfecho, igl, pres, sexo, EDAD)
s1<-filter(s1, igl>0 & pres>0)

# Etiquetar
s1$igl<-factor(s1$igl, levels =c(1,2,3,4), labels=c("Mucha", "Algo", "Poco", "Nada"))
s1$sexo<-factor(s1$sexo, levels =c(1,2), labels=c("Hombre", "Mujer"))
s1$pres<-factor(s1$pres, levels =c(1,2), labels=c("Aprueba", "No aprueba"))

s1$satisfecho<-factor(s1$satisfecho, levels =c(0,1), labels=c("No Satisfecho", "Satisfecho"))
s1$edad<-as.numeric(s1$EDAD)

reg.log <- glm(satisfecho ~ igl+pres + sexo+edad, 
                    data = s1, family = "binomial")
summary(reg.log)


#5.3 Resultados: momios

momios<-exp(coefficients(reg.log))%>%data.frame()
View(momios)

#-------------------------------------------
# RETO-2:
# Ajusta un modelo de regresión para
# algún país (IDENPA):
# IDENPA  32  Argentina
# IDENPA  76  Brasil
# IDENPA  724 España
# IDENPA  858 Uruguay
#-------------------------------------------

