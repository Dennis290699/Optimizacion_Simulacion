# Simulación de 10 lanzamientos de una moneda equilibrada
set.seed(123)
moneda <- sample(c("Cara", "Cruz"), 10, replace = TRUE)

# Contar resultados
resultado <- table(moneda)

# Mostrar resultados
print("Resultados de lanzamientos de una moneda:")
print(resultado)

# Visualización
barplot(resultado, main = "Resultados de lanzamientos de una moneda",
        xlab = "Resultado", ylab = "Frecuencia", col = c("lightblue", "lightcoral"))
