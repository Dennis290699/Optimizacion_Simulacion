# Definir la función f(x)
f <- function(x) {
  return(-5 * x^2 + 3 * x - 2)
}

# Usar optim para encontrar el máximo con restricciones usando L-BFGS-B
result_max <- optim(par = 0,                        # Punto inicial
                    fn = function(x) -f(x),         # Negar f(x) para maximizar
                    method = "L-BFGS-B",            # Método con restricciones
                    lower = -0.5,                   # Límite inferior
                    upper = 0.5)                    # Límite superior

# Mostrar el resultado
cat("Máximo encontrado en x =", result_max$par, "\n")
cat("Valor máximo de f(x) =", f(result_max$par), "\n")

# Graficar la función y el punto óptimo
x_vals <- seq(-1, 1, length.out = 100)
y_vals <- f(x_vals)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Optimización con restricciones",
     xlab = "x", ylab = "f(x)")

points(result_max$par, f(result_max$par), col = "red", pch = 19, cex = 1.5)
legend("topright", legend = "Máximo restringido", col = "red", pch = 19)
