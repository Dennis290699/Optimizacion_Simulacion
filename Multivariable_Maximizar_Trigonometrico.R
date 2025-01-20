# Generar la secuencia de valores
x1 <- seq(-10, 10, length.out = 100)
x2 <- seq(-10, 10, length.out = 100)

# Definir la función a ser optimizada
f <- function(x1, x2) {
  sin(x1) + sin(x2)
}

# Establecer un punto inicial para la optimización
initial_values <- c(0, 2.5)  # Valores iniciales para x1 y x2

# Realizar la optimización usando optim()
result <- optim(initial_values, function(x) f(x[1], x[2]), control = list(fnscale = -1))  # Maximizar

# Resultados de la optimización
optimal_values <- result$par  # Valores óptimos encontrados
optimal_value <- -result$value  # Valor óptimo de la función

# Imprimir resultados
cat("Valores óptimos: x1 =", optimal_values[1], ", x2 =", optimal_values[2], "\n")
cat("Valor óptimo de f(x1, x2):", optimal_value, "\n")

# Aplicar outer para generar la matriz z
z <- outer(x1, x2, f)

# Graficar la superficie 3D usando persp()
persp(x1, x2, z, theta = 30, phi = 30, col = "lightblue", 
      xlab = "x1", ylab = "x2", zlab = "f(x1,x2)", 
      main = "Superficie de f(x1, x2)")

persp(x1, x2, z, theta = 30, phi = 30, col = "magenta", 
      xlab = "x1", ylab = "x2", zlab = "f(x1,x2)", 
      main = "Superficie de f(x1, x2)", 
      expand = 0.5)
