rm(list=ls()) #sirve para borrar el ambiente

#Llamamos los 2 (o 3) paquetes que descargamos anteriormente
library(lme4)
library(lmerTest)
library(readxl)

#Especificamos el directorio con el que vamos a estar trabajando (se puede hacer
#desde el menú session ó poniéndola entre paréntesis con el comando setwd())

setwd("~/Library/CloudStorage/OneDrive-UNED/Curso LMMs Doctorado UNED/Sesión 2")

#Creamos una nueva variable para nuestros datos.

datos_inter <- read.csv("Data_interaccion.csv")
datos_inter <- read_excel("Data_interaccion.xlsx")

#De nuevo, por cuestiones de practicidad/tiempo, se harán menos modelos de
#los requeridos.

#Empezamos haciendo los modelos para el IF15.

m15_0 = lmer(Licks15 ~ (1|Subjects),     
           data = datos_inter)   

summary(m15_0)

m15_1 = lmer(Licks15 ~ Group + (1|Subjects),     
             data = datos_inter)   

summary(m15_1)

m15_2 = lmer(Licks15 ~ Group + Phase + (1|Subjects),     
             data = datos_inter)   

summary(m15_2)

m15_3 = lmer(Licks15 ~ Group*Phase + (1|Subjects),     
             data = datos_inter)   

summary(m15_3)

#hacemos la prueba de razón de verosimilitud

anova(m15_0,m15_1,m15_2,m15_3)

#EL mejor modelo es m15_3
summary(m15_3)

#Hacemos lo mismo con los licks del IF 30

m30_0 = lmer(Licks30 ~ (1|Subjects),     
             data = datos_inter)   

summary(m30_0)

m30_1 = lmer(Licks30 ~ Group + (1|Subjects),     
             data = datos_inter)   

summary(m30_1)

m30_2 = lmer(Licks30 ~ Group + Phase + (1|Subjects),     
             data = datos_inter)   

summary(m30_2)

m30_3 = lmer(Licks30 ~ Group*Phase + (1|Subjects),     
             data = datos_inter)   

summary(m30_3)

anova(m30_0,m30_1,m30_2,m30_3)

#EL mejor modelo es m30_3
summary(m30_3)

#Y hacemos lo mismo con los licks del IF 60

m60_0 = lmer(Licks60 ~ (1|Subjects),     
             data = datos_inter)   

summary(m60_0)

m60_1 = lmer(Licks60 ~ Group + (1|Subjects),     
             data = datos_inter)   

summary(m60_1)

m60_2 = lmer(Licks60 ~ Group + Phase + (1|Subjects),     
             data = datos_inter)   

summary(m60_2)

m60_3 = lmer(Licks60 ~ Group*Phase + (1|Subjects),     
             data = datos_inter)   

summary(m60_3)

anova(m60_0,m60_1,m60_2,m60_3)

#Ninguno modelo es mejor que los demás, excepto por una pequeña diferencia 
#en los AICs, por lo que, por cuestiones del taller, tomaremos el m60_3, para
#hacer la comparación. 

summary(m60_3)

