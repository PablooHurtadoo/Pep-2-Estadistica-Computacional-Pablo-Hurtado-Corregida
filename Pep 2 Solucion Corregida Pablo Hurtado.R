# ============================================================
# PEP2 - Estadística Computacional
# ============================================================

# ============================================================
# LIBRERÍAS
# ============================================================
library(ggplot2)

# ============================================================
# PREGUNTA 1 - Prueba Chi-cuadrado
# ============================================================

# Tabla de contingencia
datos <- matrix(c(92, 28, 78, 42, 43, 17),
                nrow = 3, byrow = TRUE,
                dimnames = list(
                  c("18 a 24 años", "25 a 34 años", "35 años o más"),
                  c("Aprobó", "Reprobó")
                ))
datos

# Visualización
df <- as.data.frame(as.table(datos))
colnames(df) <- c("Rango_Edad", "Resultado", "Frecuencia")

ggplot(df, aes(x = Rango_Edad, y = Frecuencia, fill = Resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Resultado del curso por rango etario",
       x = "Rango de Edad", y = "Frecuencia") +
  theme_minimal()

# Prueba Chi-cuadrado
prueba_chi <- chisq.test(datos)
prueba_chi

# Frecuencias esperadas (deben ser >= 5)
prueba_chi$expected

# Resultado
cat("Estadístico Chi-cuadrado:", prueba_chi$statistic, "\n")
cat("Grados de libertad:", prueba_chi$parameter, "\n")
cat("Valor p:", prueba_chi$p.value, "\n")

if (prueba_chi$p.value < 0.05) {
  cat("Conclusión: Se rechaza H0. Existe relación significativa entre rango etario y resultado.\n")
} else {
  cat("Conclusión: No se rechaza H0. No hay evidencia de relación entre las variables.\n")
}

# ============================================================
# PREGUNTA 2 - Regresión Lineal Múltiple (mtcars)
# ============================================================

data(mtcars)

# Exploración inicial
str(mtcars)
summary(mtcars)

# Correlaciones con mpg
sort(cor(mtcars)[,"mpg"], decreasing = TRUE)

# Visualización: peso vs mpg
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Peso del vehículo vs Consumo (mpg)",
       x = "Peso (1000 lbs)", y = "Millas por galón") +
  theme_minimal()

# Visualización: hp vs mpg
ggplot(mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "darkorange", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Potencia del motor vs Consumo (mpg)",
       x = "Potencia (hp)", y = "Millas por galón") +
  theme_minimal()

# Modelo de regresión lineal múltiple
modelo <- lm(mpg ~ wt + hp, data = mtcars)
summary(modelo)

# Verificación de supuestos
par(mfrow = c(2, 2))
plot(modelo)
par(mfrow = c(1, 1))

# Intervalos de confianza de los coeficientes
confint(modelo)

# Interpretación
cat("\n--- Interpretación del modelo ---\n")
cat("Intercepto:", coef(modelo)[1], "\n")
cat("Coef. wt:", coef(modelo)[2], "-> Por cada 1000 lbs adicionales, mpg disminuye en", abs(coef(modelo)[2]), "\n")
cat("Coef. hp:", coef(modelo)[3], "-> Por cada HP adicional, mpg cambia en", coef(modelo)[3], "\n")
cat("R² ajustado:", summary(modelo)$adj.r.squared, "\n")

# ============================================================
# PREGUNTA 3 - Comparación de grupos (ToothGrowth)
# ============================================================

data(ToothGrowth)

# Exploración inicial
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$supp)

# Estadísticos descriptivos por grupo
tapply(ToothGrowth$len, ToothGrowth$supp, summary)
tapply(ToothGrowth$len, ToothGrowth$supp, sd)

# Visualización
ggplot(ToothGrowth, aes(x = supp, y = len, fill = supp)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Crecimiento dental por tipo de suplemento",
       x = "Suplemento", y = "Longitud del diente (len)") +
  theme_minimal()

# Verificación de normalidad (Shapiro-Wilk por grupo)
shapiro_OJ <- shapiro.test(ToothGrowth$len[ToothGrowth$supp == "OJ"])
shapiro_VC <- shapiro.test(ToothGrowth$len[ToothGrowth$supp == "VC"])

cat("Shapiro-Wilk OJ: W =", shapiro_OJ$statistic, "| p =", shapiro_OJ$p.value, "\n")
cat("Shapiro-Wilk VC: W =", shapiro_VC$statistic, "| p =", shapiro_VC$p.value, "\n")

# Verificación de homogeneidad de varianzas
var_test <- var.test(len ~ supp, data = ToothGrowth)
var_test
cat("Varianzas iguales (p > 0.05):", var_test$p.value > 0.05, "\n")

# Decisión: paramétrico o no paramétrico
if (shapiro_OJ$p.value >= 0.05 & shapiro_VC$p.value >= 0.05) {
  
  cat("\nAmbos grupos siguen distribución normal. Se aplica prueba t.\n")
  var_equal <- var_test$p.value >= 0.05
  prueba <- t.test(len ~ supp, data = ToothGrowth, var.equal = var_equal)
  prueba
  
  cat("Estadístico t:", prueba$statistic, "\n")
  cat("Valor p:", prueba$p.value, "\n")
  cat("IC 95%:", prueba$conf.int, "\n")
  
} else {
  
  cat("\nAlgún grupo no sigue distribución normal. Se aplica prueba de Wilcoxon.\n")
  prueba <- wilcox.test(len ~ supp, data = ToothGrowth)
  prueba
  
  cat("Estadístico W:", prueba$statistic, "\n")
  cat("Valor p:", prueba$p.value, "\n")
}

# Conclusión final
if (prueba$p.value < 0.05) {
  cat("\nConclusión: Se rechaza H0. Existe diferencia significativa en el crecimiento dental entre OJ y VC.\n")
} else {
  cat("\nConclusión: No se rechaza H0. No hay diferencia significativa entre los suplementos.\n")
}