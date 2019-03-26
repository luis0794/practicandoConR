# setwd('/home/ltobar/Documentos/gitPersonal/practicandoConR/redesNeuronales/introModelosClasificacion')

# ------------------------------------------------ #
# --------- PREPARANDO CONJUNTO DE DATOS --------- #
# ------------------------------------------------ #

dataset <- read.csv("data/pulsar_stars.csv")

# Visualizando data
str(dataset)
head(dataset)

# Asignando nombre a columna
colnames(dataset)[9] <- "TipoEstrella"
# Convirtiendo a variable categorica
dataset$TipoEstrella <- factor(dataset$TipoEstrella, levels = c("0", "1"), labels = c("NoPulsar", "Pulsar"))

# Resumen estadístico de las variables del dataset:
summary(dataset)

dataset[, c(1:8)] <- scale(dataset[, c(1:8)])
summary(dataset)

# Se crea un conjunto de entrenamiento (75%) y un conjunto de validación (25%) a partir de nuestro dataset original
library(caTools)
set.seed(1234)
split <- sample.split(dataset$TipoEstrella, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# A fin de asegurarnos de que la proporción de estrellas pulsares y no pulsares es aproximadamente la misma en ambos conjuntos de datos, veamos la distribución de los tipos de estrella en cada uno:
table(training_set$TipoEstrella)
table(test_set$TipoEstrella)

# --------------------------------------- #
# --------- REGRESION LOGISTICA --------- #
# --------------------------------------- #


# La Regresión Logística es un tipo de modelo de regresión que se emplea para predecir variables categóricas, es decir, relaciones 
# entre variales independientes y una variable dependiente que solo puede tomar un valor entero y finito de clases. En términos 
# generales, la regresión logística calcula las probabilidades de ocurrencia de alguna de las clases del modelo a partir del uso de la función logística.


# Para crear y entrenar nuestro modelo de regresión logística, vamos a hacer uso de la función glm() del paquete stats:
clasificadorRL <- glm(TipoEstrella ~ ., family = binomial, data = training_set)
summary(clasificadorRL)

# Una vez que el clasificador está entrenado, podemos usarlo para predecir tanto las clases del conjunto de entrenamiento como de validación. Ya que la regresión logística ofrece como resultado las probabilidades de ocurrencia de cada clase, vamos a tomar como umbral el valor de 0.5 de modo que cualquier valor por encima de esa probabilidad se tome como 1 o Sano, y cualquier valor por debajo como 0 o Enfermo:
pred_train <- predict(clasificadorRL, type = 'response', ndata = training_set)
pred_train <- ifelse(pred_train > 0.5, 1, 0)
pred_train <- factor(pred_train, levels = c("0", "1"), labels = c("NoPulsar", "Pulsar"))

# Construyendo la matriz de confusión para evaluar la calidad de la predicción:
matrizConfusion <- table(training_set$TipoEstrella, pred_train)
matrizConfusion
# Para el caso del conjunto de entrenamiento, tendremos un total de 13150 predicciones correctas, mientras que habrá un total de 273 predicciones incorrectas. Esto nos da una precisión de predicción de 0.979 y un error de predicción de 0.02.


# Aplicando el clasificador al conjunto de validación:
pred_valid <- predict(clasificadorRL, type = 'response', newdata = test_set)
pred_valid <- ifelse(pred_valid > 0.5, 1, 0)
pred_valid <- factor(pred_valid, levels = c("0", "1"), labels = c("NoPulsar", "Pulsar"))
matrizConfusion <- table(test_set$TipoEstrella, pred_valid)
matrizConfusion
# Tenemos 4377 predicciones correctas, y 98 incorrectas, lo que representa una precisión de predicción de 97.8% y un error de predicción del 2.2%.


# Haciendo uso del paquete ROCR podemos graficar la curva ROC, la cual nos da una idea de la calidad del modelo a partir de las relaciones entre Falsos Positivos (False Positives) y Verdaderos Positivos (True Positives) obtenidos sobre el conjunto de validación:
library(ROCR)
pred1 <- prediction(as.numeric(pred_valid), as.numeric(test_set$TipoEstrella))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
# Como podemos ver, la línea resultante está bastante alejada de la diagonal que, para este tipo de curvas, representa la selección completamente al azar de las clases, lo que refleja entonces un buen desempeño del clasificador para este conjunto de datos. A fin de comparar este con los demás clasificadores a implementar, nos basaremos en el estudio de los resultados de clasificación con el conjunto de validación, así como la curva ROC.
