# Definimos los valores de x1 y x2
x1 <- seq(0.1, 0.9, by = 0.02)
x2 <- seq(0.1, 0.9, by = 0.02)

# Definimos la función
f <- function(x1, x2) {
  1/x1 + 1/x2 + (1 - x2)/(x2 * (1 - x1)) + 1/((1 - x1) * (1 - x2))
}

# Generamos la matriz de valores usando outer
z <- outer(x1, x2, FUN = Vectorize(f))

# Graficamos la superficie 3D
persp(x1, x2, z, theta = 45, phi = 20, col = "lightblue", 
      xlab = "x1", ylab = "x2", zlab = "f(x1,x2)", 
      main = "Superficie de f(x1, x2)")

# Definimos la función para optimización
f_optim <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  1/x1 + 1/x2 + (1 - x2)/(x2 * (1 - x1)) + 1/((1 - x1) * (1 - x2))
}

# Realizamos la optimización
result1 <- optim(c(0.5, 0.5), f_optim)
print(result1)
