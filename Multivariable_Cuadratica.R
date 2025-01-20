# Definimos los valores de x1 y x2
x1 <- x2 <- seq(-1.2, 1, by = 0.1)

# Definimos la función de Rosenbrock
fr <- function(x1, x2) {
  100 * (x2 - x1^2)^2 + (1 - x1)^2
}

# Generamos la matriz de valores usando outer
z <- outer(x1, x2, FUN = Vectorize(fr))

# Graficamos la superficie 3D
persp(x1, x2, z, theta = 150, phi = 20, col = "magenta", 
      xlab = "x1", ylab = "x2", zlab = "f(x1,x2)", 
      main = "Superficie de Rosenbrock")

# Definimos la función para optimización
fr_optim <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  100 * (x2 - x1^2)^2 + (1 - x1)^2
}

# Realizamos la optimización
result2 <- optim(c(-1.2, 1), fr_optim)
print(result2)
