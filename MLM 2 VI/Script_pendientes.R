rm(list=ls()) 

library(lme4)
library(lmerTest)
library(readxl)

setwd("~/Library/CloudStorage/OneDrive-UNED/Curso LMMs Doctorado UNED/Sesión 2")

datos_hist <- read.csv("Data_pendientes.csv")
datos_hist <- read_excel("Data_pendientes.xlsx")

#Vemos que las fases son varibales numéricas, pero necesitamos que sean 
#categóricas, para ello utilizamos el comando "as.character", como se ve aquí:

datos_hist$Phase <- as.character(datos_hist$Phase)

#Hacemos nuestros modelos, empezando con el nulo y subiendo la complejidad
#Por cuestiones prácticas/ilustrativas del taller vamos a dejar de lado las
#sesiones, pero al hacer un análisis completo se recomienda incluirlas también
#como posibles efectos aleatorios.

mod0 <- lmer(Licks15 ~ (1|Subjects), 
                 data = datos_hist)

summary(mod0)

mod1 <- lmer(Licks15 ~ Group + (1|Subjects), 
             data = datos_hist)

summary(mod1)

mod2 <- lmer(Licks15 ~ Phase + (1|Subjects), 
             data = datos_hist)

summary(mod2)

mod3 <- lmer(Licks15 ~ Group + Phase + (1|Subjects), 
             data = datos_hist)

summary(mod3)

#Ahora, estos datos tiene una característica que los anteriores no tenían, 
#que es que hay datos de ambos sujetos en las dos fases, es decir, dos mediciones
#con sitintos niveles de una VI, por lo que podemos calcular pendientes. 
#En este caso, pendiente entre fases en función de cada sujeto.
#Se pone primero el fijo "Phase", y después el aleatorio "Subjects".

mod4 <- lmer(Licks15 ~ Group + Phase + (1 + Phase|Subjects), 
                                         data = datos_hist)

summary(mod4)

#Hacemos la prueba de razón de verosimilitu para determinar el mejor modelo

anova(mod0,mod1,mod2,mod3,mod4)

#Y vemos que el mejor es el mod4.

summary(mod4)

#con la función coef podemos ver las pendientes que se generaron.
coef(mod4)





