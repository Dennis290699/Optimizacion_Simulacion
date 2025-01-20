# Definir la función f(x)
f <- function(x) {
  return(-5 * x^2 + 3 * x - 2)
}

# Definir la derivada de f(x)
f_prime <- function(x) {
  return(-10 * x + 3)
}

# Usar optim para encontrar el máximo con el método BFGS y la derivada
result_max <- optim(par = 0,
                    fn = function(x) -f(x),
                    gr = function(x) -f_prime(x),
                    method = "BFGS")

# Mostrar el resultado
cat("Máximo encontrado en x =", result_max$par, "\n")
cat("Valor máximo de f(x) =", f(result_max$par), "\n")

# Graficar la función y el punto óptimo
x_vals <- seq(-1, 1, length.out = 100)
y_vals <- f(x_vals)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Optimización de f(x) = -5x^2 + 3x - 2",
     xlab = "x", ylab = "f(x)")

points(result_max$par, f(result_max$par), col = "red", pch = 19, cex = 1.5)
legend("topright", legend = "Máximo", col = "red", pch = 19)