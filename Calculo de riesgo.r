rm(list=ls())
set.seed(1234)
#Libreria para leer excel.
library(readr)
#Archivo de muestra.
cv111 <- read_csv("C:/Users/DLSR_33/Desktop/03-Semana i/cv111BIS.csv")
###==========================================================================###
#Modelo lineal para ver que variable afecta el target (clientes que tengan buen
#historial creiditcio)
modelo.lineal <- lm(formula = Target ~.,
                    data = cv111)
summary(modelo.lineal)
###==========================================================================###
#Separo en my vars aquellas variables que tienen significancia en el modelo.
my.vars <- c("Target",
             "currentbalance",
             "moral",
             "estadocivil", 
             "tiempoNVivienda",
             "edad",
             "gredad",
             "edadp")
#Creo un nuevo data frame
cv111.r <- cv111[my.vars]
###==========================================================================###
#Estamar usando una parte del dataframe como entrenamiento (training)
# y otra parte como validacion.

# Entrenamiento
TrainingRows <- sample(1:nrow(cv111.r), 
                       700, #muestra de 700 filas sin remplazo.
                       replace = FALSE)
Training <- cv111.r[TrainingRows,] 

# Validacion
Valid <- cv111.r[-TrainingRows,] #notese el uso de "-" para el complemento

#prueba del modelo lineal con los datos de entrenamiento.
modelo.lineal.Training <- lm(formula = Target ~ .,
                             data = Training) #regresion con los dato de entrenamiento.

TargettPred <- predict(modelo.lineal.Training, 
                       Valid) #preduccion con los datos de validacion
#definir el datafame con el observado y predecir valores para comparar.
actual.pred <- data.frame(cbind(actual = Valid$Target, 
                                predicted = TargettPred)) 
head(actual.pred)
# Matriz de correlación
corr.accuracy <- cor(actual.pred)
corr.accuracy

###==========================================================================###
# Nota: Cuando comparamos valores observados y predecidos se podria usar el
#indicador min-max o el MAPE (Mean Absolute Percentage Error); para este caso
# MAPE no es adecuado porque algunos valores son igual a cero, por lo que se
#usa MAE (Mean Absolute Error)
min.max.accuracy <- mean(apply(actual.pred, 1, min)/apply(actual.pred, 1, max))
min.max.accuracy # = -Inf
mape <- mean(abs((actual.pred$predicted - actual.pred$actual))/actual.pred$actual) 
mape # = Inf
mae <- mean(abs((actual.pred$predicted - actual.pred$actual))) 
mae

###==========================================================================###
# Ajuste con la funcion logistica.

# Los valores ajustado de la variable "Target" se extraen del objeto 
# modelo.lineal.Training como se ve a continuacion:
z = modelo.lineal.Training$fitted.values

###==========================================================================###
## Aplicamos la funcion logistica a z.

Prob = 1/(1+exp(-z))
score = 10*Prob+500
plot(z,Prob,
     type = "l",
     col="red")
## Valor de la funcion logistica en la observación numero 100
Prob[100] 

no.riesgo <- Prob[Prob>.6] ## cutoff a .6
no.riesgo
#Subseting logico ¿se cumple o no?
length(no.riesgo) # 545 observaciones de 700 no tienen riesgo

###==========================================================================###
# Valor AIC
#Eligiendo el mejor valor usando AIC Value
library(MASS)
modelo.lineal.max <- lm( formula = Target ~., 
                         data= cv111 )

modelo.lineal.aic <- stepAIC(modelo.lineal.max, 
                             direction = "both")
summary(modelo.lineal.aic)

modelo.lineal.aic.back <- stepAIC(modelo.lineal.max, 
                                  direction = "backward")
summary(modelo.lineal.aic.back)
