
#--------------------------------------------------------------------------------
#                       MANEJO DE BASES DE DATOS 1
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------


# OBJETIVO: En este m�dulo se presenta una introducci�n R y sus caracteristicas
#           principales.


#               CONTENIDO

#       0. Preparar entorno de trabajo
#       1. Librer�as
#           1.1 Instalar (S�lo una vez)
#           1.2 Cargar (Siempre)
#       2. Directorio de trabajo
#       3. Tipos de datos
#           3.1 Asignaci�n
#           3.2 Validaci�n
#           3.3 Reasignaci�n
#       4. Estructuras de datos
#           4.1 Vectores
#           4.2  Matrices
#           4.3  Tabla (Data Frames)
#       5. Funciones
#           5.1  Funciones predefinidas
#           5.2  Definir propias
#       6.  Estructuras de control
#           6.1 IF-ELSE
#           6.2 FOR


#--------------------------------------------------------------------------------

#0.  Preparar entorno de trabajo


rm(list=ls())                 # Quitar elementos guardados (Environment)
graphics.off()                # Limpiar el espacio para las graficas (Plots)
# Ctrl + L                    # Limpia la consola (Console)


#1. Librer�as

    #1.1 Instalar (S�lo una vez)
    
    install.packages("foreign")   # Importar/exportar archivos en diversos formatos
    install.packages("questionr") # Tablas ponderadas
    install.packages("ggplot2")   # Crear gr�ficas

    #1.2 Cargar (Siempre)
    
    library(foreign)                 
    library(questionr)              
    library(ggplot2)            



#2.  Directorio de trabajo

getwd()                   # Directorio actual
setwd("C:/Error/")        # Cambiar directorio (usar " / ")



#3.  Tipos de datos

    #3.1 Asignaci�n
    
    x<-3.1416                 # Num�rico
    class(x)
    
    y<-"Curso R"              # Caracter
    class(y)


    #3.2 Validaci�n
    
    is.numeric(x)
    is.numeric(y)
    
    r<-is.numeric(x)          # Valores l�gicos (TRUE/FALSE)
    class(r)

    #3.3 Reasignaci�n
    
    x<-as.character(x)
    class(x)

    
    
#--------------------------------------
# NOTA:
# Para  vincular variables y valores
# se utiliza el s�mbolo:  <-  
    
# Saber el tipo de dato es fundamental
# para manejar bases de datos.
    
#--------------------------------------
    
    
    


#4. Estructuras de datos
    
    #4.1 Vectores
    
    num_1 <- c(3.14, 1.61, 0, -1)           # Vector caracteres
    num_1[2]                                # Acceder a lo que tiene el vector en la posici�n 2
    
    
    caracter<-c("A", "B", "C","D","E") 
    caracter[3:5]



    #4.2  Matrices
    
    v_1 <- c(78, 2, 3)
    v_2 <- c(10, 63, 0)
    
    matriz_1 <- rbind(v_1, v_2)             # rbind sirve para unir vectores (filas)
    matriz_1                                
    dim(matriz_1)
    
    matriz_2 <- cbind(v_1, v_2)             # rbind sirve para unir vectores (columnas)
    matriz_2
    
    
    matriz_2*(10)
    
    matriz_2[1,]                            # La ,  sirve para diferenciar entre columnas y filas
    matriz_2[,1]
    
    matriz_2[2,2]


#--------------------------------------
# NOTA:
# Para crear vectores se debe utilizar
# los caracteres: c()  
#--------------------------------------




    #4.3  Tabla (Data Frames)
    
    nombre<-c("Luisa","Ana","Miriam","Pedro")               # Vector de texto
    sexo<-c("M", "M", "M", "H")                             
    edad<-c(23, 45, 19, 65)                                 # Vector de n�meros enteros
    ingreso<-c(23.50, 450.01, 58.89, 789.85)                # Vector de n�meros con decimales (flotantes/racionales)
    nacion<-c("Mx", "Ur", "Mx", "Pa" )
    
    t <- data.frame(nombre, sexo,edad,ingreso,nacion)       # data.frame une vectores con distintas caracter�sticas


    #--------------------------------------
    # RETO:
    # �Qu� tienen en com�n los 5 vectores
    # que acabamos de unir?
    #--------------------------------------
    
    dim(t)                              # dim indica el tama�o de la tabla
    
    t[3 , ]                             # Las filas se ubican a la IZQUIERDA de la ,
    
    t[ , 3]                             # Las columnas se ubican a la DERECHA de la ,
    
    t[1:2 , ]                           # Con : se genera una secuencia. A la IZQUIERDA de la "," son filas
    
    t[ , 1:2]                           # Con : se genera una secuencia, A la DERECHA de la "," son columnas
    
    t[ , c("ingreso")]                  # Las columnas se pueden seleccionar por su nombre
    
    t[ , c("sexo","nacion")]            # Se puede seleccionar m�s de una columna
    
    
    
    #----------------------------------------
    # RETO:
    # De la tabla t selecciona la fila 1 y 2,
    # as� como la variable nombre e ingreso.
    #----------------------------------------
    
    t.nueva<-t[ , c("nombre") ]         # La selecci�n se asigna a una nueva base
    

#--------------------------------------
# NOTA:
# La diferencia entre una matriz y un 
# Data Frame es que las matrices solo 
# pueden tener un �nico tipo de datos.
# Observa lo que pasa con el siguiente
# ejemplo:

# d1 <- c(NA, 2, 3)
# d2 <- c("A", "B", 0)
# p <- rbind(d1, d2)
#--------------------------------------



#5.  Funciones

# La estructura b�sica de las funciones es la siguiente:
#   nombre <- function(argumentos){  Operaciones }

    #5.1  Funciones predefinidas
    
    nrow(t)                                # Numero de renglones
    ncol(t)                                # Numero de columnas
    names(t)
    
    table(t$sexo)
    wtd.table(t$nacion)

    
    
    #5.2  Definir propias
    
    mi_funcion_1<-function(x){              # i.    Defino el nombre y argumento
        x<-x*100                            # ii.   PROCESO: El valor inicial se multiplica por 100
        return(x);                          # iii.  RESULTADO: Se regresa el valor nuevo
    }

    t$edad
    mi_funcion_1(t$edad)

    
    
#6.  Estructuras de control
    
    
    #6.1 IF-ELSE
    
    # if(condici�n) {
    #     operaciones si la condici�n es verdadera (TRUE)
    # } else {
    #     operaciones si la condici�n es falsa (FALSE)
    # }
    
    
    r<-100
    
    if(r > 5) {                                 # i.  Definir argumento y evaluar la condici�n    
        print("r es mayor a 100")               # ii. PROCESO si la la condici�n es verdadera
    } else {
        print("r es menor a 100")               # iii. PROCESO si la la condici�n es falsa
    }

    #6.2 FOR
    
    #  for(elemento in objeto) {
    #     operacion_con_elemento
    #  }
    
    
    for(j in 1:5) {                             # i.  Definir secuencia y contador
        print(j)                                # ii. PROCESO imprime el contador
    }
    
    
    #----------------------------------------
    # RETO:
    # Copia el bucle que est� en la p�gina
    # http://www.sigma161.com/R-intro/ y 
    # c�rrelo, �cu�l es la diferencia?
    #----------------------------------------
    
    
    
    #----------------------------------------
    # RETO:
    # Usando la funci�n for, imprime 100 veces
    # tu nombre
    #----------------------------------------
    
  
    
    
    