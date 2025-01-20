# Definir la función f(x) = x * sin(4 * x)
f <- function(x) {
  return(x * sin(4 * x))#- para cambiar retorno a maximo
}
# Definir la función para maximizar (negativo de f)
#neg_f <- function(x) {
#  return(-f(x))  # Negar la función para maximizar
#}

# Encontrar el mínimo local usando optimización en el rango [0, 2*pi]
result_min <- optim(par = 1,               # Valor inicial para el mínimo
                    fn = f,                # Función a optimizar
                    method = "L-BFGS-B",   # Método de optimización con límites
                    lower = 0,             # Límite inferior del rango
                    upper = 4 * pi)        # Límite superior del rango

# Para encontrar el máximo, optimizamos la función negativa (-f(x)), así cambiamos el problema
# de minimizar f(x) a maximizar f(x).
result_max <- optim(par = 1,               # Valor inicial para el máximo
                    fn = function(x) -f(x),# Negar la función para buscar el máximo
                    method = "L-BFGS-B",   # Método de optimización con límites
                    lower = 0,             # Límite inferior del rango
                    upper = 2 * pi)        # Límite superior del rango

# Mostrar resultados de la optimización (mínimo)
print("Mínimo encontrado:")
print(result_min)

# Mostrar resultados de la optimización (máximo)
print("Máximo encontrado:")
print(result_max)

# Graficar la función y los puntos óptimos (máximo y mínimo) encontrados
x_vals <- seq(0, 2 * pi, length.out = 100)  # Valores de x en el rango [0, 2*pi]
y_vals <- f(x_vals)                         # Calcular los valores de f(x) para cada x

# Crear la gráfica de la función
plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2, 
     main = "Gráfico de f(x) = x * sin(4x)", xlab = "x", ylab = "f(x)")

# Añadir el mínimo encontrado en la gráfica
points(result_min$par, result_min$value, col = "red", pch = 19, cex = 1.5)

# Añadir el máximo encontrado en la gráfica
points(result_max$par, -result_max$value, col = "green", pch = 19, cex = 1.5)

# Añadir leyenda
legend("topright", legend = c("Mínimo", "Máximo"), col = c("red", "green"), pch = 19)
result_max <- optim(par = 2.5,               # Valor inicial para el máximo
                    fn = function(x) -f(x),# Negar la función para buscar el máximo
                    method = "L-BFGS-B",   # Método de optimización con límites
                    lower = 0,             # Límite inferior del rango
                    upper = 2 * pi)        # Límite superior del rango



# Mostrar resultados de la optimización (máximo)
print("Máximo encontrado:")
print(result_max)

# Graficar la función y los puntos óptimos (máximo y mínimo) encontrados
x_vals <- seq(0, 2 * pi, length.out = 100)  # Valores de x en el rango [0, 2*pi]
y_vals <- f(x_vals)                         # Calcular los valores de f(x) para cada x

# Crear la gráfica de la función
plot(x_vals, y_vals, type = "l", col = "blue", lwd = 2, 
     main = "Gráfico de f(x) = x * sin(4x)", xlab = "x", ylab = "f(x)")

# Añadir el máximo encontrado en la gráfica
points(result_max$par, -result_max$value, col = "green", pch = 19, cex = 1.5)
# Añadir el máximo encontrado en la gráfica
points(result_min$par, result_min$value, col = "red", pch = 19, cex = 1.5)

# Añadir leyenda
legend("topright", legend = c("Mínimo", "Máximo"), col = c("red", "green"), pch = 19)


result2<-optim(par = 2.5,fn = f,method ="BFGS", hessian=TRUE,lower = 0,upper=4*pi)#cambar intervalos

print(result2)

optimize(interval=c(5,2*pi),f)
optimize(interval=c(0.5,2*pi),f)
optimize(interval=c(1.5,2*pi),f)
optimize(interval=c(5,2*pi),neg_f)
optimize(interval=c(0.5,2*pi),neg_f)
optimize(interval=c(1.5,2*pi),neg_f)
