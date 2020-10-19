#--------------------------------------------------------------------------------
#                                 GRÁFICAS
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Graficar datos de la ECOVID-ML 

# MATERIAL: Encuesta Telefónica sobre COVID-19 y Mercado Laboral, INEGI.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Gráficas (ggplot)
#           4.1 Con una variable discreta
#           4.2 Con dos variables discretas
#           4.3 Con una variable continua
#           4.4 Una variable continua y una discreta
#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls())                  
graphics.off()                 
options(warn=-1)              


#1. Cargar librerías

library(foreign)                 
library(questionr)              
library(ggplot2) 
library(dplyr) 


#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/30. FLACSO/Introducción a R/Datos")



#3.  Importar datos

  #---------------------------------------
  # RETO-3:
  # Importa la base de la "ecovid0420.dbf" 
  # y nombrala como: encovid 
  #---------------------------------------

encovid<-data.frame(read.dbf("ecovid0420.dbf"))


#4. Gráficas (ggplot)
    
#4.1 Con una variable discreta

# Comentario:
#
class(encovid$PB1)    
ggplot(encovid,aes(PB1))+geom_bar(fill="blue")

# Comentario:
#
ggplot(encovid,aes(PB1))+geom_bar(fill="blue")+
    xlab("Sexo")+
    ylab("Personas")+
    ggtitle("Comparación entre hombres y mujeres")


#4.2 Con dos variables discretas

#------------------------------------------
# NOTA:
# Se pueden usar variables para construir
# las gráficas
#------------------------------------------

g1<-ggplot(encovid, aes(POS_OCU))

g1+geom_bar(fill="yellowgreen")+
  facet_wrap(~ PB1)+                          # facet_wrap divide a la pantalla
  ggtitle("Ocupación por sexo")+
  xlab("Posición de la ocupación")+
  ylab("Personas")


g2<-ggplot(encovid,aes(x=POS_OCU,fill=PB1))
g2+geom_bar(aes(weights=encovid$FAC_PER))+    # weights pondera la base para tener datos reales
  ggtitle("Ocupación por sexo")+
  xlab("Posición de la ocupación")+
  ylab("Personas")


g3<-ggplot(encovid,aes(x=POS_OCU,fill=PB1))
g3+geom_bar(aes(weights=encovid$FAC_PER),position = "dodge")+
  ggtitle("Ocupación por sexo")+              # position ajusta la posición de las barras
  xlab("Posición de la ocupación")+
  ylab("Personas")



#4.3 Con una variable continua

class(encovid$PB2)                            
encovid$PB2<-as.numeric(encovid$PB2)
class(encovid$PB2)

#------------------------------------------
# NOTA:
# Antes de graficar, verificar el tipo de
# variable
#------------------------------------------


g4<-ggplot(encovid, aes(PB2))                 # g4<- Definimos la base

# Agregamos diferentes tipos de gráficas

g4+geom_area(stat="bin")

g4+geom_freqpoly()

g4+geom_histogram(binwidth = 5)


#4.4 Una variable continua y una discreta

g5<-ggplot(encovid, aes(PB2, colour=PB1))     # colour indica que hay grupos (2 en este caso)

g5+geom_freqpoly()

g5+geom_freqpoly()+
  ggtitle("Edad por sexo")+
  xlab("Edad")+
  ylab("Personas")+
  theme(plot.title = element_text(hjust = 0.5))
                                              # theme ajusta el centrado


  #-----------------------------------------------------
  # RETO-4:
  # Con las siguientes variables: 
  # Sexo            (1=Hombre, 2=Mujer)       PB1
  # Horas trabajadas  -continua-              PE10
  #
  # Gráfica Horas trabajadas por Sexo
  #-----------------------------------------------------


  #-----------------------------------------------------
  # RETO-5:
  # Con las siguientes variables: 
  # Sexo            (1=Hombre, 2=Mujer)       PB1
  # Ocupado         (1=Ocupado, 2=Desocupado) CLASE1
  #
  # Gráfica Ocupación por Sexo
  #-----------------------------------------------------




