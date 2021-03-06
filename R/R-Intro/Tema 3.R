#--------------------------------------------------------------------------------
#                                 GR�FICAS
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Graficar datos de la ECOVID-ML 

# MATERIAL: Encuesta Telef�nica sobre COVID-19 y Mercado Laboral, INEGI.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librer�as
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Gr�ficas (ggplot)
#           4.1 Con una variable discreta
#           4.2 Con dos variables discretas
#           4.3 Con una variable continua
#           4.4 Una variable continua y una discreta
#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo

rm(list=ls())                  
graphics.off()                 
options(warn=-1)              


#1. Cargar librer�as

library(foreign)                 
library(questionr)              
library(ggplot2) 
library(dplyr) 


#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de M�xico A.C/5. Proyectos/2020/30. FLACSO/Introducci�n a R/Datos")



#3.  Importar datos

  #---------------------------------------
  # RETO-3:
  # Importa la base de la "ecovid0420.dbf" 
  # y nombrala como: encovid 
  #---------------------------------------

encovid<-data.frame(read.dbf("ecovid0420.dbf"))


#4. Gr�ficas (ggplot)
    
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
    ggtitle("Comparaci�n entre hombres y mujeres")


#4.2 Con dos variables discretas

#------------------------------------------
# NOTA:
# Se pueden usar variables para construir
# las gr�ficas
#------------------------------------------

g1<-ggplot(encovid, aes(POS_OCU))

g1+geom_bar(fill="yellowgreen")+
  facet_wrap(~ PB1)+                          # facet_wrap divide a la pantalla
  ggtitle("Ocupaci�n por sexo")+
  xlab("Posici�n de la ocupaci�n")+
  ylab("Personas")


g2<-ggplot(encovid,aes(x=POS_OCU,fill=PB1))
g2+geom_bar(aes(weights=encovid$FAC_PER))+    # weights pondera la base para tener datos reales
  ggtitle("Ocupaci�n por sexo")+
  xlab("Posici�n de la ocupaci�n")+
  ylab("Personas")


g3<-ggplot(encovid,aes(x=POS_OCU,fill=PB1))
g3+geom_bar(aes(weights=encovid$FAC_PER),position = "dodge")+
  ggtitle("Ocupaci�n por sexo")+              # position ajusta la posici�n de las barras
  xlab("Posici�n de la ocupaci�n")+
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

# Agregamos diferentes tipos de gr�ficas

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
  # Gr�fica Horas trabajadas por Sexo
  #-----------------------------------------------------


  #-----------------------------------------------------
  # RETO-5:
  # Con las siguientes variables: 
  # Sexo            (1=Hombre, 2=Mujer)       PB1
  # Ocupado         (1=Ocupado, 2=Desocupado) CLASE1
  #
  # Gr�fica Ocupaci�n por Sexo
  #-----------------------------------------------------




