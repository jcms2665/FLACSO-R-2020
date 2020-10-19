#--------------------------------------------------------------------------------
#                          BASES DE DATOS - REPASO
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Manejo de datos con tidyverse y analisis estadístico básico

# MATERIAL: Encuesta Intercensal 2015, INEGI


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Cargar librerías
#       2. Directorio de trabajo
#       3. Importar datos
#       4. Exploración inicial
#       5. Etiquetar variables
#       6. Datos muestrales y ponderados
#       7. Recodificación de variables
#           7.1 Tipo de dato y formato
#           7.2 Medidas de tendencia central 
#           7.3 Mutate y missings
#           7.4 Categorías
#       8. Subconjuntos de datos con tidyverse 
#           8.1 Variables 
#           8.2 Casos 
#           8.3 Varibles y casos


#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo


rm(list=ls())                 # 
graphics.off()                # 
options(warn=-1)              # Evita que aparezcan warnings


#1. Cargar librerías

library(foreign)                 
library(questionr)              
library(ggplot2) 
library(car)
library(dplyr)
library(tidyverse)

#---------------------------------------
# NOTA:
# Si alguna librería no está instalada
# utiliza el comando install.packages
# para descargarla. Hecho esto, intenta
# llamarla nuevamente
#---------------------------------------


#2.  Directorio de trabajo

setwd("D:/OneDrive - El Colegio de México A.C/5. Proyectos/2020/30. FLACSO/Análisis estadístico/Datos/eic2015_09_csv")

#---------------------------------------
# NOTA:
# Si aparece un error, revisa el tipo
# de diagonal que tienes. 
# Se debe usar "/" o "//"
#---------------------------------------


#3.  Importar datos

ei<-data.frame(read.csv("TR_PERSONA09.csv"), sep=",")

#4.  Exploración inicial

dim(ei)                                   # Dimensión de la base 

attach(ei)                                # Se fija la base de datos

    # Alcaldías
    wtd.table(NOM_MUN)
    
    # Sexo
    wtd.table(SEXO)
    
    # Lengua indígena
    wtd.table(PERTE_INDIGENA)

detach(ei)
    
    
#5.  Etiquetar variables

ei$SEXO <- factor(ei$SEXO,levels = c(1,3),labels = c("Hombre","Mujer"))
ei$PERTE_INDIGENA <- factor(ei$PERTE_INDIGENA,levels = c(1,2,3,8,9),labels = c("Sí","Sí, en parte","No","No sabe","No especificado"))


#6. Datos muestrales y ponderados

wtd.table(ei$SEXO)                                # Tabulado con datos muestrales

wtd.table(ei$SEXO, weights =ei$FACTOR)            # Tabulado con datos ponderados

t1<-wtd.table(ei$SEXO, weights =ei$FACTOR)        

prop.table(t1)                                    # Proporciones



#-------------------------------------------
# RETO-1:
# Obtén un tabulado ponderado  con la
# variable PERTE_INDIGENA y la frecuencia
# de cada una de sus categorías
#-------------------------------------------

wtd.table(ei$PERTE_INDIGENA, weights =ei$FACTOR)
t2<-wtd.table(ei$PERTE_INDIGENA, weights =ei$FACTOR)
prop.table(t2)


#7. Recodificación de variables

    #7.1 Tipo de dato y formato
    
    class(ei$EDAD)
    ei$EDAD <-as.numeric(as.character(ei$EDAD))
    class(ei$EDAD)
    
    
    #7.2 Medidas de tendencia central 
    
    #-------------------------------------------
    # NOTA:
    # Con la variable EDAD, queremos crear 3
    # categorías en donde se distribuya la 
    # población proporcionalmente.
    #
    # ¿Cómo eligo los rangos?
    #-------------------------------------------


    min(ei$EDAD)
    
    max(ei$EDAD)
    
    mean(ei$EDAD)
    
    median(ei$EDAD)
    
    #7.3 Mutate y missings
    
    ei<-mutate(ei,EDAD.N=ifelse(EDAD<999,EDAD,NA))          # mutate agrega una variable con ciertas condiciones
    
    
    max(ei$EDAD.N, na.rm =TRUE)
    
    quantile(ei$EDAD.N, na.rm=TRUE)
    
    quantile(ei$EDAD.N, na.rm=TRUE, prob=c(0,0.33,0.66,1))  # Con prob se agrega la probabilidad deseada
    
    #7.4 Categorías
    
    ei$r_edad[ei$EDAD.N>=0 & ei$EDAD.N<=22] <- 1      
    ei$r_edad[ei$EDAD.N>=23 & ei$EDAD.N<=43] <- 2
    ei$r_edad[ei$EDAD.N>=44 & ei$EDAD.N<=110] <- 3
    
    wtd.table(ei$r_edad)
    
    wtd.table(ei$r_edad, weights = ei$FACTOR)%>%prop.table()
    
    
#8. Subconjuntos de datos con tidyverse 

    #8.1 Variables 
    
    ei.mini.1<-ei %>% select(SEXO, EDAD,PERTE_INDIGENA)
    
    #8.2 Casos 
    
    ei.mini.2<-filter(ei, SEXO=="Mujer")
    
    #8.3 Varibles y casos
    
    ei.mini.3<-filter(ei, SEXO=="Mujer") %>% select(EDAD,PERTE_INDIGENA)



    #---------------------------------------------------
    # RETO-2:
    # 1.  Con la base ei, genera una nueva base (ei.4) 
    #     en donde estén las variables de: el último 
    #     año o grado aprobado y nombre del municipio. 
    # 2.  Esta nueva base SÓLO
    #     debe tener a quienes sí se consideran
    #     indígenas. 
    #---------------------------------------------------


    ei.mini.4<-filter(ei, PERTE_INDIGENA=="Sí") %>% select(NOM_MUN,NIVACAD)


