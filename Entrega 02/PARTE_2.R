# ============================================================
# PROYECTO: SISTEMA DE EVALUACION Y CLASIFICACION
# DE RIESGO CREDITICIO MEDIANTE R
# PARTE II - MVP
# Daniela Lepe Araya
# ============================================================
#CARGAR LIBRERIAS
library(rsample) # Division de datos en entrenamiento y prueba
library(pROC)    # Evaluacion del modelo mediante curva ROC y AUC
# ============================================================
#CARGAR BASE DE DATOS
datos <- read.csv("cs-training.csv")
dim(datos)

#EXPLORACION INICIAL DE LOS DATOS 

#Ver nombres de las variables
names(datos)
#Ver las primeras 6 obs. 
head(datos)
#Revisar estructura de la base de datos 
str(datos)

#REVISION DE DATOS FALTANTES

#Contar valores faltantes (NA) por variable
colSums(is.na(datos))

#Calcular porcentaje de datos faltantes por variable
round(colMeans(is.na(datos))*100,2)
#Revisar medianas de las variables con datos faltantes 
median(datos$MonthlyIncome, na.rm = TRUE)
#Revisar mediana del numero de dependientes
median(datos$NumberOfDependents, na.rm = TRUE)

#TRATAMIENTO DE DATOS FALTANTES 

#Reemplazar ingresos faltantes por la mediana 
datos$MonthlyIncome[is.na(datos$MonthlyIncome)] <-
  median (datos$MonthlyIncome, na.rm =TRUE)

#Reemplazar dependientes faltantes por la mediana
datos$NumberOfDependents[is.na(datos$NumberOfDependents)] <-
  median(datos$NumberOfDependents, na.rm = TRUE)

#Comprobar que ya no existan valores faltantes
colSums(is.na(datos))

#ELIMINAR VARIABLE DE IDENTIFICACION

#La variable X corresponde al numero de registro y no aporta info,
#util para predecir el riesgo 
datos$X <- NULL
#Comprobar variables restantes
names(datos)
#Comprobar nuevas dimensiones, de 12 a 11.
dim(datos)

#ANALISIS DESCRIPTIVO 

#Obtener resumen estadistico de las variables
summary(datos)

#REVISION DE VALORES ATIPICOS 

#Revisar registros con edad igual a 0 y menor a 18 años
sum(datos$age == 0)
sum(datos$age < 18)

#Revisar valores presentes en las variables de atrasos
table(datos$NumberOfTimes90DaysLate)
table(datos$NumberOfTime30.59DaysPastDueNotWorse)
table(datos$NumberOfTime60.89DaysPastDueNotWorse)

#Identificar registros con valores 96 o 98 
sum(datos$NumberOfTimes90DaysLate %in% c(96,98))
sum(datos$NumberOfTime30.59DaysPastDueNotWorse %in% c(96, 98))
sum(datos$NumberOfTime60.89DaysPastDueNotWorse %in% c(96,98))

#Comprobar si los valores 96 y 98 son los mismos en las 3 variables
#de atraso
sum(
    datos$NumberOfTimes90DaysLate %in% c(96,98) &
    datos$NumberOfTime30.59DaysPastDueNotWorse %in% c(96, 98) &
    datos$NumberOfTime60.89DaysPastDueNotWorse %in% c(96,98)
)

#LIMPIEZA DE VALORES ATIPICOS

#Eliminar registro con edad igual a 0
datos <- datos[datos$age > 0,]
#Eliminar registros con valores 96 o 98
#en cualquiera de las variables de atrasos
datos <- datos[
    !(datos$NumberOfTimes90DaysLate %in% c(96,98) |
      datos$NumberOfTime30.59DaysPastDueNotWorse %in% c(96, 98) |
      datos$NumberOfTime60.89DaysPastDueNotWorse %in% c(96,98)),
]
#Comprobar dimensiones de la base limpia
dim(datos)
#Compobar que los valores fueron eliminados 
sum(datos$age == 0)
sum(datos$NumberOfTimes90DaysLate %in% c(96, 98))

#ANALISIS DE LA VARIABLE OBJETIVO 

#Cantidad de clientes por clase 
table(datos$SeriousDlqin2yrs)
# % de clientes por clase 
prop.table(table(datos$SeriousDlqin2yrs))*100

# La variable objetivo presenta un desbalance de clases:
# aproximadamente 93,4% corresponde a clase 0
# y 6,6% corresponde a clase 1.
# Este desbalance se considerara en la evaluacion del modelo.

#DIVISION EN ENTRENAMIENTO Y PRUEBA

#Convertir la variable objetivo en factor (Binomial)
datos$SeriousDlqin2yrs <- factor(datos$SeriousDlqin2yrs, 
levels=c(0,1))
#Fijar semilla para obtener resultados reproducibles
set.seed(123)
#Division estratificada: 80% entrenamiento y 20% prueba
division <- initial_split(
    datos, 
    prop = 0.80,
    strata = SeriousDlqin2yrs
)
#Crea las dos bases de datos
entrenamiento <- training(division)
prueba <- testing(division)
#Revisar dimensiones
dim(entrenamiento)
dim(prueba)

#COMPROBAR DISTRIBUCION DE LAS CLASES

#Distribucion en entrenamiento 
prop.table(table(entrenamiento$SeriousDlqin2yrs))*100
#Distribucion en prueba 
prop.table(table(prueba$SeriousDlqin2yrs))*100

#MODELO DE REGRESION LOGISTICA // MODELO 1

#Entrenar modelo de regresion logistica
modelo_logistico <- glm(SeriousDlqin2yrs ~.,
                        data = entrenamiento, 
                       family = binomial
                       )

#Revisar resultados del modelo
summary(modelo_logistico)

#GENERAR PROBABILIDADES EN EL CONJUNTO DE PRUEBA

#Estimar probabilidades de riesgo para cada cliente

probabilidades <- predict(
  modelo_logistico,
  newdata = prueba, 
  type = "response"
)
#Revisar primeras probabilidades 
head(probabilidades)
#resumen de las probabilidades estimadas
summary(probabilidades)

#CLASIFICACION CON UMBRAL 0.50

#Convertir probabilidades en claseS
prediccion <- ifelse(probabilidades >= 0.50, 1, 0)
#convertir resultado en factor 
prediccion <- factor(prediccion, levels =c(0,1))
#Revisar cantidad de predicciones por clase
table(prediccion)
#Revisar porcentaje de predicciones por clase
prop.table(table(prediccion))*100

#MATRIZ DE CONFUSION - UMBRAL 0.50

#comparar clasificacion predicha con clasificacion real 
matriz_confusion <- table(
  Real =prueba$SeriousDlqin2yrs,
  predicho = prediccion
)
#mostrar matriz
matriz_confusion

#METRICAS DEL MODELO - UMBRAL 0.50

#Extraer valores de la matriz de confusion
VN <- matriz_confusion[1,1]
FP <- matriz_confusion[1,2]
FN <- matriz_confusion[2,1]
VP <- matriz_confusion[2,2]

#Accuracy % total de clasificciones correctas 
accuracy <- (VP + VN)/ (VP + VN + FP+ FN)
#Sensibilidad 
sensibilidad <- VP / (VP +FN)
#eSPECIFICIDAD 
especificidad <- VN /(VN +FP)
#Precision 
precision <- VP / (VP + FP)
#Mostrar todas las metricas en porcentaje
round(c(
  Accuracy = accuracy,
  Sensibilidad = sensibilidad,
  Especificidad = especificidad,
  Precision = precision
) * 100, 2)

#EVALUACION DE DIFERENTES UMBRALES

#Clasificar segun el umbral seleccionado
evaluar_umbral <- function(umbral) {
  pred <- ifelse (probabilidades >= umbral, 1, 0)
#Convertir los valores reales a formato numerico
  real <- as.numeric(as.character(prueba$SeriousDlqin2yrs))
  VP <- sum(pred == 1 & real == 1)
  VN <- sum(pred == 0 & real == 0)
  FP <- sum(pred == 1 & real == 0)
  FN <- sum(pred == 0 & real == 1)
#Calcular componentes de la matriz de confusion
  accuracy <- (VP + VN) / (VP + VN + FP + FN)
  sensibilidad <- VP / (VP + FN)
  especificidad <- VN / (VN + FP)
  precision <- VP / (VP + FP)
#Resultados
  return(c(
    Umbral = umbral,
    Accuracy = accuracy,
    Sensibilidad = sensibilidad,
    Especificidad = especificidad,
    Precision = precision
  ))
}
#Evaluar distintos umbrales
resultados_umbrales <- rbind(
  evaluar_umbral(0.50),
  evaluar_umbral(0.40),
  evaluar_umbral(0.30),
  evaluar_umbral(0.20),
  evaluar_umbral(0.10)
)
#Convertir metricas a porcentaje
resultados_umbrales[, 2:5] <- resultados_umbrales[, 2:5] * 100
round(resultados_umbrales, 2)

#CURVA ROC Y AUC 

#Crear curva ROC 
roc_modelo <- roc(prueba$SeriousDlqin2yrs, probabilidades)
#Calcular AUC 
auc_modelo <- auc(roc_modelo)
#Graficar curva ROC
plot( roc_modelo, 
  main = "Curva ROC - Modelo de Riesgo Crediticio")

#SELECCION DE UMBRAL MEDIANTE CURVA ROC

#Obtener umbral segun criterio de Youden
umbral_roc <- coords(
  roc_modelo,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

#Mostrar resultado
umbral_roc

#CLASIFICACION CON UMBRAL ROC

#Definir umbral seleccionado mediante el indice de Youden
umbral_final <- 0.0633552

#Clasificar las probabilidades utilizando el nuevo umbral
prediccion_final <- ifelse(
  probabilidades >= umbral_final,1,0)
# Convertir a factor
prediccion_final <- factor(
  prediccion_final,
  levels = c(0, 1))

# Crear nueva matriz de confusion
matriz_final <- table(
  Real = prueba$SeriousDlqin2yrs,
  Predicho = prediccion_final)

# Mostrar matriz
matriz_final

#METRICAS CON UMBRAL FINAL

# Extraer valores de la nueva matriz
VN_final <- matriz_final[1,1]
FP_final <- matriz_final[1,2]
FN_final <- matriz_final[2,1]
VP_final <- matriz_final[2,2]

# Calcular metricas
accuracy_final <- (VP_final + VN_final) /
                  (VP_final + VN_final + FP_final + FN_final)

sensibilidad_final <- VP_final / (VP_final + FN_final)
especificidad_final <- VN_final / (VN_final + FP_final)
precision_final <- VP_final / (VP_final + FP_final)

# Mostrar resultados en porcentaje
round(c(
  Accuracy = accuracy_final,
  Sensibilidad = sensibilidad_final,
  Especificidad = especificidad_final,
  Precision = precision_final
) * 100, 2)

#COMPARACION MODELO INICIAL VS UMBRAL ROC

comparacion_modelos <- data.frame(
  Metrica = c(
    "Accuracy",
    "Sensibilidad",
    "Especificidad",
    "Precision"
  ),
  
  Umbral_050 = c(
    accuracy,
    sensibilidad,
    especificidad,
    precision
  ) * 100,
  
  Umbral_ROC = c(
    accuracy_final,
    sensibilidad_final,
    especificidad_final,
    precision_final
  ) * 100
)

# Redondear resultados
comparacion_modelos[, 2:3] <-
  round(comparacion_modelos[, 2:3], 2)

# Mostrar tabla
comparacion_modelos

#PRODUCTO MINIMO VIABLE (MVP)
#EVALUACION DE UN NUEVO CLIENTE

#Crear cliente ficticio para demostrar el funcionamiento del modelo
cliente_nuevo <- data.frame(
  RevolvingUtilizationOfUnsecuredLines = 0.45,
  age = 40,
  NumberOfTime30.59DaysPastDueNotWorse = 1,
  DebtRatio = 0.35,
  MonthlyIncome = 6500,
  NumberOfOpenCreditLinesAndLoans = 5,
  NumberOfTimes90DaysLate = 0,
  NumberRealEstateLoansOrLines = 1,
  NumberOfTime60.89DaysPastDueNotWorse = 0,
  NumberOfDependents = 2
)

# Ver datos del cliente
cliente_nuevo

#Calcular probabilidad estimada para el nuevo cliente
prob_cliente <- predict(
  modelo_logistico,
  newdata = cliente_nuevo,
  type = "response")

# Mostrar probabilidad
prob_cliente
# Mostrar probabilidad en porcentaje
round(prob_cliente * 100, 2)

#CLASIFICACION AUTOMATICA DEL CLIENTE

#Clasificar al cliente utilizando el umbral seleccionado
clasificacion_cliente <- ifelse(
  prob_cliente >= umbral_final,1,0)
# Mostrar clasificacion
clasificacion_cliente

#Mostrar resultado del MVP

cat(
  "\n--- EVALUACION DE RIESGO CREDITICIO ---\n",
  "Probabilidad estimada:", round(prob_cliente * 100, 2), "%\n",
  "Umbral utilizado:", round(umbral_final * 100, 2), "%\n",
  "Clasificacion:", clasificacion_cliente, "\n"
)
#RESULTADO FINAL DEL MVP
#Generar mensaje final de evaluacion

if (clasificacion_cliente == 1) {
  resultado <- "CLASE 1 - Requiere mayor evaluacion"
} else {
  resultado <- "CLASE 0 - No clasificado en dificultad financiera"
}
cat(
  "\n=====================================\n",
  "   EVALUACION DE RIESGO CREDITICIO\n",
  "=====================================\n",
  "Probabilidad estimada:", round(prob_cliente * 100, 2), "%\n",
  "Umbral utilizado:", round(umbral_final * 100, 2), "%\n",
  "Resultado:", resultado, "\n",
  "=====================================\n"
)