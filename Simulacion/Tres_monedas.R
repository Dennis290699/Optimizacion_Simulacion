# Simulación de tres monedas lanzadas simultáneamente
set.seed(123)
lanzamientos <- 10
monedas <- matrix(sample(c(1, 0), lanzamientos * 3, replace = TRUE), ncol = 3)

# Contar número de caras en cada lanzamiento
caras_por_lanzamiento <- rowSums(monedas)

# Convertir en data.frame
monedas_df <- as.data.frame(monedas)
colnames(monedas_df) <- c("Moneda1", "Moneda2", "Moneda3")
monedas_df$caras_por_lanzamiento <- caras_por_lanzamiento

# Mostrar resultados
print("Tabla con caras por lanzamiento:")
print(monedas_df)

# Visualización
barplot(monedas_df$caras_por_lanzamiento, main = "Caras por lanzamiento",
        xlab = "Experimentos", ylab = "Caras", col = "blue")

# Contar resultados
resultado <- table(monedas_df$caras_por_lanzamiento)

# Mostrar resultados
print("Resultados de lanzamientos de tres monedas:")
print(resultado)

# Visualización
barplot(resultado, main = "Resultados de lanzamientos de tres monedas",
        xlab = "Resultado", ylab = "Frecuencia", col = "green")
        