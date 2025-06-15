# ============================================================================
# EDA COMPLETO - FACTORES DE ÉXITO EN PROYECTOS AGILE
# ============================================================================

# Cargar librerías necesarias
library(readxl)
library(PerformanceAnalytics) 
library(corrplot)
library(psych)
#library(VIM)
#library(e1071)


# ========================== EDA 1: EXPLORACIÓN INICIAL ==========================

# Cargar datos
#agile_data <- read_excel("Agile_Projects_Dataset.xlsx")

# Cambiar nombre

agile_data <- Agile_Projects_Dataset

# 1.1 Visualización inicial
head(agile_data, 5)
tail(agile_data, 3)

# 1.2 Dimensiones y estructura
dim(agile_data)
str(agile_data)
names(agile_data)

# 1.3 Estadísticos descriptivos completos
summary(agile_data)
describe(agile_data)  # psych package - más detallado


# ========================== EDA 2: AJUSTE DE VARIABLES ==========================

# 2.1 Verificar nombres (ya están bien en tu dataset)
names(agile_data)

# 2.2 Verificar tipos de datos
sapply(agile_data, class)

# 2.3 Convertir Project_Success a factor si es necesario
agile_data$Project_Success <- as.factor(agile_data$Project_Success)


# ========================== EDA 3: DATOS AUSENTES ==========================

# 3.1 Verificar datos ausentes
sum(is.na(agile_data))
colSums(is.na(agile_data))

# 3.2 Patrón de datos ausentes (si los hay)
# aggr(agile_data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE)

# ========================== EDA 4: IDENTIFICACIÓN DE DATOS ATÍPICOS ==========================
#Desviacion Estandar
desviaciones = apply(agile_data, 2, sd)
print(desviaciones)


# 4.1 Seleccionar solo variables predictoras
predictoras <- agile_data[,1:5]  # Las 5 primeras columnas

# 4.2 Método básico: Boxplots (tu método)
boxplot(predictoras, 
        main="Detección de Outliers - Variables Predictoras",
        names=c("Agile", "Risk", "Management", "Supply", "Time"),
        las=2)

# 4.3 Método avanzado: Distancia de Mahalanobis (de la guía)
# Calcular distancia de Mahalanobis
maha_dist <- mahalanobis(predictoras, 
                         colMeans(predictoras), 
                         cov(predictoras))

# Punto de corte (distribución chi-cuadrado)
cutoff <- qchisq(0.975, df = ncol(predictoras))

# Identificar outliers
outliers <- which(maha_dist > cutoff)
print(paste("Outliers detectados con Mahalanobis:", length(outliers)))
print(paste("Casos outliers:", paste(outliers, collapse=", ")))

# 4.4 Visualizar outliers
plot(maha_dist, 
     main="Distancia de Mahalanobis",
     ylab="Distancia", 
     xlab="Observación")
abline(h=cutoff, col="red", lwd=2)


# ========================== EDA 5: CORRELACIONES ==========================

# 5.1 Correlaciones con la variable objetivo (ÉXITO DEL PROYECTO)
cor_with_success <- cor(predictoras, as.numeric(agile_data$Project_Success))
print("Correlaciones con Éxito del Proyecto:")
print(round(cor_with_success, 3))

# VER los valores primero
print("Correlaciones con Éxito del Proyecto:")
print(round(cor_with_success, 3))

# Graficar correctamente
barplot(as.vector(cor_with_success), 
        names.arg = c("Agile", "Risk", "Management", "Supply", "Time"),
        main="Correlación con Éxito del Proyecto",
        ylab="Correlación",
        las=2,
        col="steelblue")



############################## CLUSTERING ######################################

### PASO 1: IDENTIFICAR LAS VARIABLES
factores_agile <- agile_data[, c("Agile_Effectiveness", "Risk_Mitigation", 
                                 "Management_Satisfaction", "Supply_Chain_Improvement", 
                                 "Time_Efficiency", "Cost_Savings")]

## PASO 2: MEDIDAS DE ASOCIACION
datos_escalados <- scale(factores_agile)
#Selecciono el método de distancia euclidiana
distancias <- dist(datos_escalados, method = "euclidean")^2 

## PASO 3: ELECCION DEL CLUSTER (JERARQUICO)
#Selecciono el clúster jerárquico (hclust) y el método Ward
hc <- hclust(distancias, method = "ward.D2")
#Visualizo el dendograma
plot(hc)
#identifico cuántos cluster se formaron
hc$merge
#Visualizo el historial de conglomeración
merge_history <- data.frame(
  Etapa = 1:nrow(hc$merge),
  Cluster1 = hc$merge[, 1],
  Cluster2 = hc$merge[, 2],
  Distancia = hc$height)
print(merge_history)

############## ANALISIS DEL EJERCICIO
#### SELECCION DEL CLUSTER
#Selecciono los cluster que usaré
clusters <- cutree(hc, k = 3)
agile_data$Cluster <- as.factor(clusters)
#Validamos los cluster elegidos
library(cluster)
sil <- silhouette(clusters, distancias)
mean_sil <- mean(sil[, 3])
cat("Coeficiente de silueta promedio:", mean_sil, "\n")
## Grafico de barras
library(factoextra)
fviz_silhouette(sil)
#Validamos con chi-cuadrado
validacion2 <- chisq.test(table(agile_data$Cluster, agile_data$Project_Success))
print(validacion2)

library(ggpubr)
# Tabla de contingencia para ver la distribución
table(agile_data$Cluster, agile_data$Project_Success)

#### ANALISIS FACTORIAL
library(psych)
# Dividimos en 2 factores
fa <- fa(datos_escalados, nfactors = 2, rotate = "varimax")
print(fa)
# Gráfico de factores
fa.diagram(fa)
# Extraemos las puntuaciones factoriales
factores <- fa$scores
# Dividimos en 2 factores
ggplot(factores, aes(x = MR1, y = MR2, color = agile_data$Cluster)) +
  geom_point() +
  labs(x = "Factor 1", y = "Factor 2", color = "Cluster") +
  theme_minimal()



