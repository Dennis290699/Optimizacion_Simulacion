# Define la función
f <- function(x) {
  return(sin(x) + x^2/10)
}

# Encuentra los máximos y mínimos locales usando la función optim
result_max <- optim(par = 0, fn = function(x) -f(x), method = "L-BFGS-B", lower = -pi, upper = pi)
result_min <- optim(par = 0, fn = f, method = "L-BFGS-B", lower = -pi, upper = pi)

# Imprime los resultados en la consola
print("Máximo local:")
print(paste("x =", result_max$par))
print(paste("f(x) =", -result_max$value))
print("")

print("Mínimo local:")
print(paste("x =", result_min$par))
print(paste("f(x) =", result_min$value))

# Crea un rango de valores para x
x_vals <- seq(-pi, pi, length.out = 100)

# Calcula los valores de f(x) para cada x
y_vals <- f(x_vals)

# Grafica la función
plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2,
     main = "Gráfico de f(x) = sin(x) + x^2/10",
     xlab = "x", ylab = "f(x)")

# Añade los puntos máximos y mínimos locales a la gráfica
points(result_max$par, f(result_max$par), col = "red", pch = 19, cex = 1.5)
points(result_min$par, f(result_min$par), col = "green", pch = 19, cex = 1.5)

# Añade una leyenda a la gráfica
legend("topright", legend = c("Máximo", "Mínimo"), col = c("red", "green"), pch = 19)