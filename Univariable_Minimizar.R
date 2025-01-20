# Definir la función f(x)
f <- function(x) (x - 2)^2 + 3

# Usar optim para encontrar el mínimo
result_min <- optim(par = 0,  # Punto inicial
                    fn = f,  # Función a minimizar
                    method = "BFGS")  # Método sin límites

# Mostrar el resultado
cat("Mínimo encontrado en x =", result_min$par, "\n")
cat("Valor mínimo de f(x) =", f(result_min$par), "\n")

# Graficar la función y el punto óptimo
x_vals <- seq(-1, 5, length.out = 100)  # Rango para x
y_vals <- f(x_vals)                     # Valores de f(x)

plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Minimización de f(x) = (x-2)^2 + 3",
     xlab = "x", ylab = "f(x)")
points(result_min$par, f(result_min$par), col = "red", pch = 19, cex = 1.5)
legend("topright", legend = "Mínimo", col = "red", pch = 19)
