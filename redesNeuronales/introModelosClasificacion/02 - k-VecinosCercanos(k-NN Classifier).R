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

# ---------------------------------------------------------- #
# --------- K - VECINOS CERCANOS (K-NN CLASSIFIER) --------- #
# ---------------------------------------------------------- #

# La clasificación por k vecinos cercanos o k-NN es un modelo de clasificación que basa su entrenamiento en el cálculo de las distancias de un nuevo dato o dato al que se desee asignar a una clase, con la mayoría de clases a las que pertenezcan sus k vecinos más cercanos, siendo k un parámetro del algoritmo.

# Para construir el clasificador k-NN vamos a hacer uso de la función knn del paquete class, y tomaremos como valor de k = 10:
library(class)
set.seed(1234)
pred_valid_knn <- knn(training_set[, -9], 
                      test_set[, -9], 
                      cl = training_set[, 9],
                      k = 10)

# Y una vez contamos con las predicciones para el conjunto de validación, podemos crear nuestra matriz de confusión:
matrizConfusion <- table(test_set$TipoEstrella, pred_valid_knn)
matrizConfusion
# Lo que nos arroja una precisión de predicción de 97.61% y un error de predicción de 2.39% para el conjunto de validación.

# Si generamos la curva ROC para este caso, obtenemos:
library(ROCR)
pred1 <- prediction(as.numeric(pred_valid_knn), as.numeric(test_set$TipoEstrella))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
# Lo que demuestra del mismo modo un buen desempeño en la clasificación de los tipos de estrella para el modelo k-NN.
