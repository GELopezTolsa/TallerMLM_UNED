rm(list=ls()) #sirve para borrar el ambiente

#Llamamos los 2 (o 3) paquetes que descargamos anteriormente
library(lme4)
library(lmerTest)
library(readxl)

#Especificamos el directorio con el que vamos a estar trabajando (se puede hacer
#desde el menú session ó poniéndola entre paréntesis con el comando setwd())

setwd("~/Library/CloudStorage/OneDrive-UNED/Curso LMMs Doctorado UNED/Sesión 2")

#Creamos una nueva variable para nuestros datos.

datos_PPR <- read.csv("Data_PPR.csv")
datos_PPR <- read_excel("Data_PPR.xlsx")

datos_PPR$IF <- as.character(datos_PPR$IF)

#Empezamos con un modelo nulo, en el que la VD solo dependa de los sujetos.
#Elegimos los sujetos, porque asumimos que variarán más que con las sesiones,
#ya que las sesiones se supone que están esables.
#Nòtese que utilizamos un nombre diferente "mo0" en lugar de "m0", esto es sólo
#para no confundirnos con el script anterior, pero si vamos a trabajar con un
#solo script no es necesario.

mo0 = lmer(PPR ~ (1|Sujeto),     
          data = datos_PPR)   

summary(mo0)

#En los siguientes modelos añadiremos cada una de nuestras variables 
#independientes por sepearado, y solo uno de nuestros efectos aleatorios.

mo1 = lmer(PPR ~ Grupo + (1|Sujeto),
                    data = datos_PPR)

summary(mo1)

mo2 = lmer(PPR ~ IF + (1|Sujeto),
           data = datos_PPR)

summary(mo2)

#Ahora creamos modelos con una VI, y el otro efecto aleatorio que tenemos 
#disponible


mo3 = lmer(PPR ~ Grupo + (1|Sesion),
           data = datos_PPR)

summary(mo3)

mo4 = lmer(PPR ~ IF + (1|Sesion),
           data = datos_PPR)

summary(mo4)

#observamos que aquí con estos dos modelos nos da un mensaje: 
#"boundary (singular) fit: see help('isSingular')"
#esto quiere decir que tenemos pocos datos para que sea un modelo confiable

#Lo sigueinte es crear modelos con cada VI, pero ambos efectos aleatorios

mo5 = lmer(PPR ~ Grupo + (1|Sesion) + (1|Sujeto),
           data = datos_PPR)

summary(mo5)

mo6 = lmer(PPR ~ IF + (1|Sesion) + (1|Sujeto),
           data = datos_PPR)

summary(mo6)

#Ahora creamos modelos con ambas VIs, y cada uno de los efectos aleatorios

mo7 = lmer(PPR ~ IF + Grupo + (1|Sujeto),
                          data = datos_PPR)

summary(mo7)

mo8 = lmer(PPR ~ IF + Grupo + (1|Sesion),
           data = datos_PPR)

summary(mo8)

#nuevamente mo8 nos da el mensaje de singularidad. 

#Por último creamos el modelo más complejo que podemos crear cone stos datos:

mo9 = lmer(PPR ~ IF + Grupo + (1|Sesion) + (1|Sujeto),
           data = datos_PPR)

summary(mo9)

#Hacemos la prueba de razón de verosimilitud (con la función "anova"). Aquí 
#podemos incluir todos los modelos, o bien, excluir los que tienen problemas de
#singularidad. De cualquier forma, esos no serán los mejores modelos.

anova(mo0,mo1,mo2,mo3,mo4,mo5,mo6,mo7,mo8,mo9)
anova(mo0,mo1,mo2,mo5,mo6,mo7,mo9)

#vemos que los modelos mo2 y mo6 son ambos muy buenos, y sus AICs son muy
#similares, por lo que podemos repetir la prueba incluyendo solo esos dos, y
#y el nulo.

anova(mo0,mo2,mo6)

#el mo2 parece ser el mejor modelo, asi que ese seleccionaremos. 

summary(mo2)

#Listo, podemos redactar estos resultados.
