## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Maquinas de Vector Soporte [Parte 1]
## 
##  4. Fuentes:
##     https://www.datacamp.com/community/tutorials/support-vector-machines-r"


## ------------------------------------------------------------------------------------
# Guardamos los OUTPUTS:
sink("OUTPUTS.txt")

# Cargamos la libreria
library(ISLR)

# Cargamos los datos:
data.OJ <- OJ
# GUardamos los datos:
write.csv(data.OJ, file = "OJ.csv", row.names = F)

# Estructura de los datos
print("# Estructura de los datos")
print(str(data.OJ))


# Comprobamos valores faltantes en la variable respuesta
print("# Comprobamos valores faltantes en la variable respuesta")
sum(is.na(OJ$Purchase))


# Distribución variable respuesta
library(ggplot2)

# Exxportamos el plot
png(filename = "DisVarResp.png")

# Graficamos la distribución variable respuesta, depende de las bebidas
ggplot(data = OJ, aes(x = Purchase, y = ..count.., fill = Purchase)) +
geom_bar() +
labs(title = "Distribución de 'Purchase'") +
scale_fill_manual(values = c("darkgreen", "orangered2"), 
                  labels = c("Citrus Hill", "Orange Juice")) +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))

dev.off()


# Tabla frecuencias variable respuesta
print("# Tabla frecuencias variable respuesta")
table(OJ$Purchase)

# Tabla proporciones variable respuesta
print("# Tabla proporciones variable respuesta")
library(dplyr)
prop.table(table(OJ$Purchase)) %>% round(digits = 2)

# Resultados:
"Para que los modelos generados sean útiles, el porcentaje de aciertos en cuanto a la clasificación de las observaciones ha de superar un nivel mínimo, en este caso, el que se obtendría si la predicción de todas las observaciones se correspondiera con la clase mayoritaria."

"La clase mayoritaria (moda) en este caso es la bebida CH con el 61% de las compras. Este será el nivel basal a superar por el modelo (este es el porcentaje mínimo de aciertos si siempre se predijera CH). (Recalcular este valor con los datos de entrenamiento)"


## ------------------------------------------------------------------------------------
# Cargamos la libreria
library(caret)

# Índices observaciones de entrenamiento
set.seed(123)
train <- createDataPartition(y = OJ$Purchase, p = 0.8, list = FALSE, times = 1)

# Datos entrenamiento
datosOJ_train <- OJ[train, ]
# Dimensión de los datos de entrenamiento
print("# Dimensión de los datos de entrenamiento")
dim(datosOJ_train)

# Datos test
datosOJ_test <- OJ[-train, ]
print("# Dimensión Datos test")
dim(datosOJ_test)


## ------------------------------------------------------------------------------------
# Cargamos la libreria
library(e1071)

# Optimización de hiperparámetros mediante validación cruzada 10-fold
set.seed(325)
tuning <- tune(svm, Purchase ~ ., data = datosOJ_train, 
               kernel = "linear", 
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 15, 20)), 
               scale = TRUE)

#Podemos acceder a los errores de validación con cada valor de coste con la función summary():
print("#Podemos acceder a los errores de validación con cada valor de coste con la función summary():")
summary(tuning)

# NOmbres del modelo
print("# NOmbres del modelo")
names(tuning)

# Exportamos la imagen:
png(filename = "Hiperpa.png")

# Graficamos el Error de Validación
ggplot(data = tuning$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  labs(title = "Error de validación ~ hiperparámetro C") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# Resultado de la grafica:
"El valor de coste que resulta en el menor error de validación (0,165) es 15."

# Almacenamos el modelo optimo obtenido y accedemos a su información
print("# Almacenamos el modelo optimo obtenido y accedemos a su información")
modelo_svc <- tuning$best.model
summary(modelo_svc)

#Resultado
"El número de vectores soporte es de 345, 173 de la clase CH y 172 de la clase MM."


#Podemos obtener los índices de las observaciones que se corresponden con los vectores soporte:

# Muestra de 50 de los 345
print("# Muestra de 50 de los 345")
head(modelo_svc$index)

#El mejor modelo obtenido sería equivalente a ajustar:

modelo_svc <- svm(Purchase ~ ., data = datosOJ_train, 
                  kernel = "linear", 
                  cost = 15, 
                  scale = TRUE)


# Nota: Al tratarse de un problema con más de dos predictores, podemos representar el modelo usando la función plot(), pero creando representaciones entre pares de predictores (teniendo en cuenta que plot.svm solo representa predictores continuos).

# Guaradmos los predictores de los modelos
pdf("predictor.pdf")

plot1 <- plot(modelo_svc, datosOJ_test, SalePriceCH ~ PriceCH)

plot2 <- plot(modelo_svc, datosOJ_test, SalePriceCH ~ SalePriceMM)

plotc <- c(plot1, plot2)

for (x in plotc) {
  x
}

dev.off()