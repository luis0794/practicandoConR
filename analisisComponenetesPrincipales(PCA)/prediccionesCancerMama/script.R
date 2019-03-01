# setwd("D:/Luis/git/practicandoConR/analisisComponenetesPrincipales(PCA)/prediccionesCancerMama/")

#### Análisis de datos ####

# Age (years)
# BMI (kg/m2)
# Glucose (mg/dL)
# Insulin (µU/mL)
# HOMA
# Leptin (ng/mL)
# Adiponectin (µg/mL)
# Resistin (ng/mL)
# MCP-1(pg/dL)
# Classification (1=Healthy controls, 2=Patients (with cancer))
data <- read.csv("data/dataR2.csv")

df <- dplyr::select(data, -Classification)

head(df, 3)

summary(df)

# install.packages("GGally")

# Gráfica con pares de variables. En esta gráfica podemos ver distribuciones, correlaciones y también gráficas de caja para ver las diferencias en pacientes con y sin cáncer de mama. 
# En esta gráfica podemos ver la diferencia de edad entre esos dos grupos, puede indicar que el grupo de control no está perfectamente elegido. Además todos los indicadores son más altos para los pacientes con cáncer.
GGally::ggpairs(data)

corr_df <- cor(df,method='pearson')

# install.packages("corrplot")
# Para obtener una mejor vista de las correlaciones, se realizó el corrplot. La correlación entre la glucosa, HOMA, leptina ahora es claramente visible.
corrplot::corrplot(corr_df)


#### Análisis de componenetes principales ####

### How to choose number of components ###
