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

# ----------------------------------------------------------------------------------- #
# --------- Máquina de Soporte Vectorial con Kernel (Kernel SVM Classifier) --------- #
# ----------------------------------------------------------------------------------- #

# Las Máquinas de Vectores de Soporte son modelos capaces de generar clasificaciones o regresiones de datos no lineales a partir de la transformación de los datos de entrada a otros espacios de mayores dimensiones. En el caso de la clasificación, la SVM busca encontrar aquella curva que sea capaz de separar y clasificar los datos de entrenamiento garantizando que la separación entre ésta y ciertas observaciones del conjunto de entrenamiento (los vectores de soporte) sea la mayor posible. Para entender mejor el concepto, recomiendo echarle un ojo al video Support Vector Machine (SVM) - Fun and Easy Machine Learning (https://www.youtube.com/watch?v=Y6RRHw9uN9o&feature=youtu.be). Por su parte, la aplicación de Kernels a los SVM consiste en valerse de funciones especiales que, de hecho, son los que llevan los datos de entrada a dimensiones superiores en donde existe separabilidad lineal y, por lo tanto, solución al problema de clasificación.


#En R, vamos a implementar la clasificación SVM haciendo uso de la función svm del paquete e1071:
library(e1071)
set.seed(1234)
clasificadorSVM <- svm(TipoEstrella ~ ., data = training_set, 
                       type = 'C-classification', kernel = 'radial')
# En este caso, los parámetros C-classification y radial hacen referencia al tipo de clasificador que se construye, y que el Kernel empleado es radial o Gaussiano.

# La predicción sobre los valores del conjunto de validación será entonces:
pred_valid_svm <- predict(clasificadorSVM, newdata = test_set)

# Lo que produce la siguiente matriz de confusión:
matrizConfusion <- table(test_set$TipoEstrella, pred_valid_svm)
matrizConfusion
# Tenemos entonces una precisión de predicción del 97.72% y un error de predicción del 2.27%. La curva ROC será:
library(ROCR)
pred1 <- prediction(as.numeric(pred_valid_svm), as.numeric(test_set$TipoEstrella))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
# Esto mejora los valores obtenidos en el caso del clasificador k-NN.


