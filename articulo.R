# ============================================================================
# EDA COMPLETO - FACTORES DE ÉXITO EN PROYECTOS AGILE (VERSIÓN MEJORADA)
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

# 4.2 Método básico: Boxplots (MEJORADO)
boxplot(predictoras, 
        main="Detección de Outliers - Variables Predictoras de Éxito Agile",
        names=c("Efectividad\nAgile", "Mitigación\nRiesgos", "Satisfacción\nGestión", "Mejora\nCadena", "Eficiencia\nTiempo"),
        las=2,
        col=c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7"),
        ylab="Puntuación (1-5)",
        cex.main=1.2)

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

# 4.4 Visualizar outliers (MEJORADO)
plot(maha_dist, 
     main="Distancia de Mahalanobis - Detección de Casos Atípicos",
     ylab="Distancia de Mahalanobis", 
     xlab="Número de Observación",
     pch=16, 
     col=ifelse(maha_dist > cutoff, "red", "steelblue"),
     cex=0.8,
     xaxt="n")  # Quitar eje x para hacer espacio
abline(h=cutoff, col="red", lwd=2, lty=2)
axis(1, at=seq(0, 200, 50))  # Agregar eje x personalizado
# Leyenda en la parte inferior
par(xpd=TRUE)  # Permitir dibujar fuera del área del gráfico
legend(x=100, y=-0.5, 
       legend=c("Casos normales", "Casos atípicos", "Límite crítico"), 
       col=c("steelblue", "red", "red"), 
       pch=c(16, 16, NA), 
       lty=c(NA, NA, 2), 
       lwd=c(NA, NA, 2),
       ncol=3,
       xjust=0.5,
       bg="white")
par(xpd=FALSE)  # Restaurar configuración


# ========================== EDA 5: CORRELACIONES ==========================

# 5.1 Correlaciones con la variable objetivo (ÉXITO DEL PROYECTO)
cor_with_success <- cor(predictoras, as.numeric(agile_data$Project_Success))
print("Correlaciones con Éxito del Proyecto:")
print(round(cor_with_success, 3))

# VER los valores primero
print("Correlaciones con Éxito del Proyecto:")
print(round(cor_with_success, 3))

# Graficar correctamente (MEJORADO)
barplot(as.vector(cor_with_success), 
        names.arg = c("Efectividad\nAgile", "Mitigación\nRiesgos", "Satisfacción\nGestión", "Mejora\nCadena", "Eficiencia\nTiempo"),
        main="Correlación de Factores con Éxito del Proyecto",
        sub="Valores cercanos a 0 indican baja correlación",
        ylab="Coeficiente de Correlación",
        las=2,
        col=ifelse(as.vector(cor_with_success) > 0, "#27AE60", "#E74C3C"),
        ylim=c(-0.1, 0.15),
        cex.names=0.8)
abline(h=0, lty=2, col="gray50")
# Agregar valores en las barras
text(x=1:5, y=as.vector(cor_with_success) + ifelse(cor_with_success > 0, 0.01, -0.01), 
     labels=round(as.vector(cor_with_success), 3), 
     cex=0.9, font=2)



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

#Visualizo el dendograma (MEJORADO)
plot(hc, 
     main="Dendrograma - Agrupación Jerárquica de Proyectos Agile",
     sub="Método Ward.D2 con distancia euclidiana",
     xlab="Proyectos Agile (200 observaciones)",
     ylab="Distancia de Agregación",
     cex=0.6,
     hang=-1)
# Agregar rectángulos para mostrar los 3 clusters
rect.hclust(hc, k=3, border=c("#FF6B6B", "#4ECDC4", "#45B7D1"))

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

# Crear nombres descriptivos para clusters basados en tus objetivos de investigación
agile_data$Cluster_Descriptivo <- factor(agile_data$Cluster, 
                                         levels = c("1", "2", "3"),
                                         labels = c("Perfil Balanceado", 
                                                    "Orientado a Eficiencia", 
                                                    "Orientado a Gestión"))

#Validamos los cluster elegidos
library(cluster)
sil <- silhouette(clusters, distancias)
mean_sil <- mean(sil[, 3])
cat("Coeficiente de silueta promedio:", mean_sil, "\n")

## Grafico de barras (MEJORADO)
library(factoextra)
# Crear gráfico de silhouette mejorado
sil_plot <- fviz_silhouette(sil, 
                            label = TRUE,
                            print.summary = FALSE) +
  labs(title = "Análisis de Calidad de Clusters - Coeficiente de Silhouette",
       subtitle = paste("Promedio:", round(mean_sil, 3), "- Estructura débil pero identificable"),
       x = "Proyectos agrupados por perfil de factores críticos",
       y = "Coeficiente de Silhouette (calidad de agrupación)") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
                    name = "Perfil de Proyecto",
                    labels = c("Perfil Balanceado (n=77)", "Orientado a Eficiencia (n=54)", "Orientado a Gestión (n=69)")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(sil_plot)

#Validamos con chi-cuadrado
validacion2 <- chisq.test(table(agile_data$Cluster, agile_data$Project_Success))
print(validacion2)

library(ggpubr)
# Tabla de contingencia para ver la distribución (MEJORADA)
tabla_contingencia <- table(agile_data$Cluster_Descriptivo, agile_data$Project_Success)
print("=== DISTRIBUCIÓN DE ÉXITO POR TIPO DE PROYECTO ===")
print(tabla_contingencia)

# Calcular tasas de éxito por cluster
tasas_exito <- prop.table(tabla_contingencia, 1) * 100
print("=== TASAS DE ÉXITO POR TIPO DE PROYECTO (%) ===")
print(round(tasas_exito, 1))

#### ANALISIS FACTORIAL
library(psych)
# Dividimos en 2 factores
fa <- fa(datos_escalados, nfactors = 2, rotate = "varimax")
print(fa)

# Gráfico de factores (MEJORADO)
fa.diagram(fa, 
           main="Análisis Factorial - Estructura de Factores en Proyectos Agile")

# Extraemos las puntuaciones factoriales
factores <- fa$scores

# Dividimos en 2 factores (GRÁFICO MEJORADO)
library(ggplot2)
factores_df <- data.frame(
  Factor_Gestion = factores[,1],  # MR1
  Factor_Eficiencia = factores[,2],  # MR2
  Perfil_Proyecto = agile_data$Cluster_Descriptivo,
  Exito_Proyecto = factor(agile_data$Project_Success, 
                          levels = c("0", "1"), 
                          labels = c("No Exitoso", "Exitoso"))
)

factor_plot <- ggplot(factores_df, aes(x = Factor_Gestion, y = Factor_Eficiencia, 
                                       color = Perfil_Proyecto, shape = Exito_Proyecto)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Mapa Factorial: Perfiles de Factores Críticos de Éxito",
       subtitle = "Distribución de proyectos según dimensiones de gestión y eficiencia",
       x = "Factor 1: Gestión y Satisfacción\n(Carga alta: Management_Satisfaction)",
       y = "Factor 2: Eficiencia Operativa\n(Carga alta: Time_Efficiency, Risk_Mitigation)",
       color = "Perfil de Proyecto",
       shape = "Resultado del Proyecto") +
  scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
  scale_shape_manual(values = c(16, 17)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(size = 4)),
         shape = guide_legend(override.aes = list(size = 4))) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray50")

print(factor_plot)

# ========================== RESUMEN INTERPRETATIVO ==========================
cat("\n=== RESUMEN INTERPRETATIVO DEL ANÁLISIS ===\n")
cat("PERFILES DE FACTORES CRÍTICOS IDENTIFICADOS:\n")
cat("• Perfil Balanceado (n=77): Rendimiento equilibrado en todos los FCE\n")
cat("• Orientado a Eficiencia (n=54): Sobresalen en eficiencia temporal y cadena de suministro\n") 
cat("• Orientado a Gestión (n=69): Altos en efectividad Agile y satisfacción de gestión\n\n")

cat("DIMENSIONES FACTORIALES IDENTIFICADAS:\n")
cat("• Factor 1 (Gestión): Principalmente Management_Satisfaction y Risk_Mitigation\n")
cat("• Factor 2 (Eficiencia): Principalmente Time_Efficiency y Agile_Effectiveness\n\n")

cat("VALIDACIÓN ESTADÍSTICA:\n")
cat("• Coeficiente Silhouette:", round(mean_sil, 3), "- Estructura débil pero identificable\n")
cat("• Chi-cuadrado p-value:", round(validacion2$p.value, 4), 
    ifelse(validacion2$p.value > 0.05, "- No hay asociación significativa con éxito\n", "- Asociación significativa con éxito\n"))

cat("\nHALLAZGOS PARA EL ARTÍCULO EMPÍRICO:\n")
cat("Los datos revelan 3 perfiles distintos de gestión de FCE en proyectos ágiles,\n")
cat("pero ninguno predice significativamente el éxito, sugiriendo que el éxito\n")
cat("depende más de la implementación que del perfil de factores adoptado.\n")
