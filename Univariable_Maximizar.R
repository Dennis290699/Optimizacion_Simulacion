# Definir la función f(x)
# f(x) = -5x^2 + 3x - 2
f <- function(x) {
  return(-5 * x^2 + 3 * x - 2)
}

# Usar optim para encontrar el máximo
# Punto inicial
# Negamos f(x) para maximizar
# Método sin límites
result_max <- optim(par = 0,
                    fn = function(x) -f(x),
                    method = "BFGS")
result_max <- optim(par = 0,                 # Punto inicial
                    fn = function(x) -f(x),  # Negamos f(x) para maximizar
                    method = "BFGS")         # Método sin límites

# Mostrar el resultado
cat("Máximo encontrado en x =", result_max$par, "\n")
cat("Valor máximo de f(x) =", f(result_max$par), "\n")

# Graficar la función y el punto óptimo
# Rango para x
x_vals <- seq(-1, 1, length.out = 100)
# Valores de f(x)
y_vals <- f(x_vals)
x_vals <- seq(-1, 1, length.out = 100)  # Rango para x
y_vals <- f(x_vals)                     # Valores de f(x)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Optimización de f(x) = -5x^2 + 3x - 2",
     xlab = "x", ylab = "f(x)")
points(result_max$par, f(result_max$par), col = "red", pch = 19, cex = 1.5)
legend("topright", legend = "Máximo", col = "red", pch = 19)
