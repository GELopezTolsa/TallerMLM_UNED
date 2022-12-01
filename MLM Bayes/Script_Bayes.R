rm(list=ls())

#Para el análisis Bayesiano se debe instalar un nuevo paquete:

install.packages("brms")

#activamos las librerías y especificamos nuestro directorio.
library(brms)
library(readxl)

setwd("/Users/gabrielaeugenia/Documents/GITHUB/TallerMLM_UNED/MLM 2 VI")

#importamos los datos

data <- read.csv("Data_interaccion.csv")
data <- read_excel("Data_interaccion.xlsx")

#### FI 15 s ####

#vemos que esta función nos pide más datos.
#chains: se refiere a la cantidad de procesos de iteración
#iteraciones: número de repeticiones (hechas por el ordenador) de ajuste al modelo.
#warmpu: cantidad de iteraciones que servirán para "entrenar" al modelo, y que 
#se descartarán de los resultaods. Típicamente la mitad de iteraciones.
#adapt_delta y max_treedepth nos ayudan a que el modelo converja más fácilmente.
#Converger, quiere decir que se encuentre una solución estable.
#Adapt_delta, mientras más cerca a 1, hará que se haga más lentamente, pero de 
#forma más precisa; y max-tredepth determinará qué tan "profunda" puede ser la 
#cadena de iteraciones.
#seed es un número aleatorio que se debe usar en todos los modelos del mimos 
#análisis, sirve como punto de partida. 
#save_all_pars guarda información que necesitaremos más adelante.

#Empezamos con el modelo nulo.

mod015 <- brm(formula = Licks15 ~ (1|Subjects), 
           data = data, chains = 4, iter = 2000, warmup = 1000,
           control = list(adapt_delta=0.99, max_treedepth = 12),
           seed = 1702, save_all_pars = TRUE)

#ojo, da un "warning", pero aún así se crea la variable en el ambiente, 
#por lo que podemos seguir trabajando. El warning es porque hay una nueva 
#versión del paquete, pero no funciona bien en esta versión de RStudio

mod015

#Y seguimos con los modelos más complejos. Todos los parámetros los dejamos
#igual que en el nulo, a menos de que diera problema en algún modelo.

mod115 <- brm(formula = Licks15 ~ Phase + (1|Subjects), 
            data = data, chains = 4, iter = 2000, warmup = 1000,
            control = list(adapt_delta=0.99, max_treedepth = 12),
            seed = 1702, save_all_pars = T)

mod115

mod215 <- brm(formula = Licks15 ~ Group + (1|Subjects),  #A = EXP, B = CONT
            data = data, chains = 4, iter = 2000, warmup = 1000,
            control = list(adapt_delta=0.99, max_treedepth = 12),
            seed = 1702, save_all_pars = T)

mod215

mod315 <- brm(formula = Licks15 ~ Phase + Group + (1|Subjects), 
            data = data, chains = 4, iter = 2000, warmup = 1000,
            control = list(adapt_delta=0.99, max_treedepth = 12),
            seed = 1702, save_all_pars = T)

mod315

mod415 <- brm(formula = Licks15 ~ Phase*Group + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

#Para evaluar cuál es el mejor modelo vamos a calcular el factor bayes para 
#cada modelo. 
#Solo podemos hacer comparaciones de dos en dos, pero si, por ejemplo, el mod215
#es mejor que el 115 y el 115 que el 015, podemos asumir que el 215 es mejor que 
#el 015.
#Un BF = 1 indica que ninguno modelo es mejor
#UN BF > 1 indica evidencia por el primer modelo (en orden dentro del paréntesis)
#Un BF < 1 evidencia por el segundo modelo.

bayes_factor(mod115,mod015)
bayes_factor(mod215,mod115)
bayes_factor(mod315,mod215)
bayes_factor(mod415,mod315)

#Al final se compara con el modelo nulo, para poner ese resultado.
bayes_factor(mod415,mod015)

#Con esta línea vamos a graficar el modelo, puede aparecer del lado derecho en 
#la pestaña "plots", o en una ventana aparte. 
#En una columna nos va a poner las dsitribuciones de cada parámetro del modelo, 
#y en la otra unas gráficas que nos indican la superposición de las cadenas.

plot(mod415)

#Este comando nos va a guardar todas las muestras, es decir, los resultados de
#cada iteración después del warmup en una variable. Vamos a necesitarlo más 
#abajo.

post_samples_mod415 = posterior_samples(mod415)
#Si llamamos a la variable, podemos ver los resultados.
post_samples_mod415

#Aquí vamos a verificar qué tanto se parecen las muestras al modelo.
#PP significa posterior predictive, y se refiere justo a las predicciones que
#forman la distribucion posterior.

ppmod415<-pp_check(mod415,nsamples = 1000)
ppmod415

#Aquí vamos a verificar qué porcentaje de nuestras muestras son mayores o 
#menores a cero (en función de la dirección de la pendiente), para hablar de 
#qué tanta evidencia hay de que esa pendiente es positiva o negativa.
#Llamamos al modelo para ver nuestras pendientes.
mod415

mean(post_samples_mod415$b_PhaseD<0)
mean(post_samples_mod415$b_GroupB>0)

#En la interacción se tiene que poner como texto (entre comillas) porque si no, 
#R se vuelve un poco loco con los ":".
mean(post_samples_mod415$"b_PhaseD:GroupB">0)


##################################################################

#Repetiríamos lo mismo para los otros datos (IF 30 e IF 60).

#### IF 30 s ####

mod030 <- brm(formula = Licks30 ~ (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod130 <- brm(formula = Licks30 ~ Phase + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod230 <- brm(formula = Licks30 ~ Group + (1|Subjects),  #A = EXP, B = CONT
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod330 <- brm(formula = Licks30 ~ Phase + Group + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod430 <- brm(formula = Licks30 ~ Phase*Group + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

bayes_factor(mod430,mod030)

plot(mod430)

post_samples_mod430 = posterior_samples(mod430)
post_samples_mod430
ppmod430<-pp_check(mod430,nsamples = 1000)
ppmod430

mod430
mean(post_samples_mod430$b_PhaseD<0)
mean(post_samples_mod430$b_GroupB<0)
mean(post_samples_mod430$"b_PhaseD:GroupB">0)



##################################################################

#### IF 60 s ####

mod060 <- brm(formula = Licks60 ~ (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod160 <- brm(formula = Licks60 ~ Phase + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod260 <- brm(formula = Licks60 ~ Group + (1|Subjects),  #A = EXP, B = CONT
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod360 <- brm(formula = Licks60 ~ Phase + Group + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

mod460 <- brm(formula = Licks60 ~ Phase*Group + (1|Subjects), 
              data = data, chains = 4, iter = 2000, warmup = 1000,
              control = list(adapt_delta=0.99, max_treedepth = 12),
              seed = 1702, save_all_pars = T)

bayes_factor(mod460,mod060)

plot(mod460)

post_samples_mod460 = posterior_samples(mod460)
post_samples_mod460
ppmod460<-pp_check(mod460,nsamples = 1000)
ppmod460

mod460

mean(post_samples_mod460$b_PhaseD<0)
mean(post_samples_mod460$b_GroupB<0)
mean(post_samples_mod460$"b_PhaseD:GroupB">0)


