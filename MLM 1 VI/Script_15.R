rm(list=ls()) 
#sirve para borrar el ambiente, es recomendable pasarla antes de 
#empezar con un nuevo script

#Instalamos los paquetes que vamos a necesitar.
#Estas líneas no es necesario correrlas cada evz que hacemos un script, 
#solo la primera vez.
#También se pueden poner directamente en la Consola.

install.packages("lme4") #nótese que va entre comillas
install.packages("lmerTest")
install.packages("readxl") #solo si se va a trabajar con excel

#Llamamos los 3 paquetes que descargamos anteriormente
library(lme4)
library(lmerTest)
library(readxl)

#Especificamos el directorio con el que vamos a estar trabajando. 
#Se puede hacer desde el menú session (recomendada) ó con el comando setwd().

setwd("~/Dropbox/aNew Statistics/Tutorial BLMM para AEC/Taller")

#Creamos una varibale para nuestros datos, recuerden usar nombres cortos, claros
#y sin espacios.

datos <- read.csv("Data_15.csv")
datos <- read_excel("Data_15.xlsx")
#Ejecutamos el código hasta aquí, observamos que se crea la variable "datos" en
#el ambiente

#Empezamos a construir nuestros modelos, para ello debemos identificar bien las
#variables que vamos a utilizar y los nombres con los que las guardamos. Podemos
#consultarlo en el ambiente, dando click en nuestra varibale "datos".

#Para construir los modelos vamos a usar la funciónn lmer, incluída en el paquete
#lme4. Las funciones tienen distintos argumentos, algunos son implícitos, como 
#la fórmula, otros los declararemos explícitamente, como el de "data". Los
#argumentos se separan por comas dentro de la función.

#Los modelos deben ir de lo más sencillo a lo más complejo, empezando con un
#modelo nulo en el que la VD sea función (~) de los sujetos como efecto aleatorio.

m0 = lmer(PPR ~ (1|Sujeto), 
              data = datos)      
                
                                 
#El primer argumento de lmer es la fórmula, que sería: "PPR ~ (1|Sujeto)" 
#en este caso estamos diciendo que la PPR es función de los interceptos 
#aleatorios para cada sujeto.
#El segundo argumento es "data", ponemos data=datos.

summary(m0)  

#Utilizamos la funciòn "summary()" para ver los resultados del modelo.

#Seguimos creando modelos más complejos, en el m1 utilizamos nuestra VI principal
#que en este caso sería el grupo, y añadiríamos interceptos aleatorios por 
#sujeto. 

m1 = lmer (PPR ~ Grupo + (1|Sujeto),
                      data = datos)

#Volvemos a usar summary para ver los resultados del modelo, y lo comparamos con
#nuestro archivo de datos original para ver que tenga sentido lo que nos está
#saliendo y descartar errores de dedo o de formato.

summary(m1)

m2 = lmer(PPR ~ Grupo + (1|Sujeto) + (1|Sesion), data = datos)

summary(m2)

#Una vez que tenemos todos los modelos que consideremos pertinentes, utilizamos
#la funciòn de "anova" (que en realidad es una prueba de reazón de verosimilitud)
#para determinar cuál es el mejor.

anova(m0,m1,m2)

#Se determinó que el mejor modelo es m1. Ahora vamos a analizar sus coeficientes

summary(m1)
coef(m1)

#Listo, podemos redactar estos resultados.