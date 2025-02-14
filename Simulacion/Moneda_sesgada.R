# Simulación de una moneda sesgada (70% Cara, 30% Cruz)
set.seed(123)
moneda_sesgada <- sample(c("Cara", "Cruz"), 10, replace = TRUE, prob = c(0.7, 0.3))

# Contar resultados
resultado_sesgado <- table(moneda_sesgada)

# Mostrar resultados
print("Resultados de la moneda sesgada:")
print(resultado_sesgado)

# Visualización
barplot(resultado_sesgado, main = "Resultados de moneda sesgada",
        xlab = "Resultado", ylab = "Frecuencia", col = c("gold", "red"))
