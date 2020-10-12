#--------------------------------------------------------------------------------
#                       MANEJO DE BASES DE DATOS 2
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Analizar la ECOVID-ML con R

# MATERIAL: Encuesta Telefónica sobre COVID-19 y Mercado Laboral, INEGI.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploración inicial
#           4.1 Etiquetar variables
#           4.1 Nuevas variables etiquetadas
#       5. Recodificar variables
#           5.1 Validar tipo de dato
#           5.2 Cambiar formato
#           5.3 Crear nueva variable 
#           5.4 Generar rangos 
#       6. Subconjuntos de datos
#           6.1 Variables (columnas)
#           6.2 Casos (filas)
#           6.3 Casos (filas) y variables (columnas)

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

#---------------------------------------
# NOTA:
# Si alguna librería no está instalada
# utiliza el comando install.packages
# para descargarla. Hecho esto, intenta
# llamarla nuevamente
#---------------------------------------


#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/30. FLACSO/Introducción a R/Datos")

#---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------


  #---------------------------------------
  # RETO:
  # ¿Qué formato tiene la base de datos?
  #---------------------------------------




#3.  Importar datos


    #3.1 Opción 1
    inegi<-data.frame(read.dbf("ecovid0420.dbf"))
    
    #3.2 Opción 2
    encovid<-read.dbf("ecovid0420.dbf") %>% data.frame()

    rm("encovid")                   # rm sirve para quitar elementos

#4.  Exploración inicial

names(inegi)
head(inegi,2)

wtd.table(inegi$PB1)


  #4.1  Etiquetar variables
  
  inegi$PB1 <- factor(inegi$PB1,levels = c(1,2),labels = c("Hombre","Mujer"))
  wtd.table(inegi$PB1)

#---------------------------------------------------------------
# NOTA:
# Para etiquetar una variable se usa la función "factor" con
# las siguientes indicaciones: 

#  i.   La variable que se va etiquetar:  inegi$PB1
#  ii.  Los valores (levels)              c(1,2)
#  iii. Las etiquetas (labels)            c("Hombre","Mujer")

#---------------------------------------------------------------

  #--------------------------------------
  # RETO:
  # Corre nuevamente la instrucción y 
  # tabula, ¿qué pasó?
  #--------------------------------------

#-------------------------------
# NOTA:
# Al etiquetar una variable, se 
# sustituye su contenido original 
# por las etiquetas
#-------------------------------



  #4.1 Nuevas variables etiquetadas
  
  inegi$ocup <- factor(inegi$CLASE2,levels = c(1,2,3,4),labels = c("Ocupado","Desocupado","Disponible","No disponible"))
  
  wtd.table(inegi$ocup, weights = inegi$FAC_PER)      # Con weights se ponderan los datos 


#5. Recodificar variables

  #5.1 Validar tipo de dato
  
  class(inegi$PB2)
  
  
  #5.2 Cambiar formato
  
  inegi$PB2 <-as.numeric(as.character(inegi$PB2))
  class(inegi$PB2)
  

  #5.3 Crear nueva variable 
  
  length(inegi)                             # lenght imprime el número de variables
  inegi$edad_recod<-0
  length(inegi)


  #5.4 Generar rangos  
  
  inegi$edad_recod[inegi$PB2 >= 0 & inegi$PB2 <=30] <- 1
  inegi$edad_recod[inegi$PB2 >= 31 & inegi$PB2 <=59] <- 2
  inegi$edad_recod[inegi$PB2 >= 60] <- 3


  wtd.table(inegi$edad_recod)


#6. Subconjuntos de datos

  #6.1 Seleccionar variables (columnas)
  
  var<-c("PB1", "POS_OCU","FAC_PER")
  inegi.1 <- inegi[,var]


  #------------------------------------------
  # RETO-1:
  # Etiqueta la variable POS_OCU 
  # 1 = Subordinados
  # 2 = Independientes
  # 1 = Sin pago
  # Tabula y compara con lo publicado
  # archivo encovid_ml_indicadores, fila 43
  #------------------------------------------




  #6.2 Seleccionar casos (filas)
  
  inegi.2 <- inegi[which(inegi$PB2<30), ]
  dim(inegi.2)
  
  #6.3 Seleccionar casos (filas) y variables (columnas)
  
  inegi.3 <- inegi[which(inegi$PB2<30), var]
  dim(inegi.3)

  
  #------------------------------------------
  # RETO-2:
  # Con la base inegi, genera una nueva base
  # que contenga mayores de 60 años y en donde
  # dolo estén las variables:
  # PB1 (sexo), PB2 (edad), PB3 (escolaridad)  
  # Guarda la base como: b1
  #------------------------------------------
  
  
  b1 <- inegi[which(inegi$PB2>=60), c("PB1","PB2","PB3")]
  
  