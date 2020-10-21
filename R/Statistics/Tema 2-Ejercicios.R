#--------------------------------------------------------------------------------
#                            MUESTRAS COMPLEJAS
#                           jcmartinez@colmex.mx
#--------------------------------------------------------------------------------

# OBJETIVO: Ejercicios de inferencia estadística

# MATERIAL: Encuesta Nacional de Ocupación y Empleo, primer trimestre
#           de 2020, INEGI.

#---------------------------------------------
# RETO-1:
# ¿Es verdad que las mujeres de 24 años
# trabajan 8 horas a la semana?
# i.    Plantea las hipótesis
# ii.   Realiza la prueba correspondiente
#       con una muestra de 17 casis
# iii.  Interpreta los resultados
#---------------------------------------------


# i.    Plantea las hipótesis

# H0: Las mujeres de 24 años trabajan 8 horas a la semana
# H1: Las mujeres de 24 años no trabajan 8 horas a la semana

# ii.   Realiza la prueba correspondiente
#       con una muestra de 17 casis

ocup.m<-sd[which(sd$sex==2 & sd$eda==24), c("hrsocup")]
ocup.muestra<-sample(ocup.m, 17)
t.test(ocup.muestra, mu=8)

# p-value = 0.08466 < 0.05

# iii.  Interpreta los resultados

# Con un 95% de confianza, rechazamos que los hombres de 18 años
# trabajan un promedio de 8 horas a la semana



#---------------------------------------------
# RETO-2:
# Aumenta el tamaño de muestra a 150
# ¿Qué ocurre?
#---------------------------------------------




#---------------------------------------------
# RETO-3:
# i.    Etiqueta la variable
# ii.   ¿Es verdad que la proporción de los
#       no asegurados es de 30%?
# iii.  Interpreta los resultados
#---------------------------------------------

n<-300
table(ssocial.muestra)
x<-168

razon<-x/n
razon

binom.test(x,n,p=0.30,alternative=c("two.sided"))

# p-value = 2.2e-16 < 0.05
# Con un 95% de confianza, rechazamos que la proporción 
# de no asegurados es del 30%



#---------------------------------------------------
# RETO-4:
# ¿Qué sucede con los adultos mayores (60 +), la
# proporción de asegurados es igual?
#---------------------------------------------------

ssocial<-sd[which(sd$clase2==1 & (sd$eda>=60 & sd$eda<98)), c("seg_soc", "sex")]

table(ssocial$sex)
table(ssocial$seg_soc, ssocial$sex)

n<-c(11150,6268)
x<-c(2189,947)  
prop.test(x,n, correct = TRUE)

# p-value = 1.026e-13< 0.05
# Con un 95% de confianza, rechazamos que la proporción 
# de asegurados entre hombres y mujeres es igual


#---------------------------------------------------
# RETO-5:
# Tener seguridad social, ¿depende del sexo de 
# las personas?
#---------------------------------------------------

#     H0: La seguridad social y el sexo son independientes
#     HA: La seguridad social y el sexo No son independientes


trab<-filter(sdemt.1, clase2==1)

trab.t<-wtd.table(trab$seg_soc, trab$sex)

chisq.test(trab.t) 

# p-value = 4.482e-05<0.05 . Las variables tienen 
# una asociación estadísticamente significativa (Rechazar H0)
