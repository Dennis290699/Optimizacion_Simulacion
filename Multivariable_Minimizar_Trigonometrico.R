# Definimos los valores de x1 y x2
x1 <- seq(-2 * pi, 2 * pi, by = 0.1)
x2 <- seq(-2 * pi, 2 * pi, by = 0.1)

# Definimos la función
f_trig <- function(x1, x2) {
  x2 * sin(x1)
}

# Generamos la matriz de valores usando outer
z1 <- outer(x1, x2, FUN = Vectorize(f_trig))

# Graficamos la superficie 3D
persp(x1, x2, z1, theta = 150, phi = 30, col = "green", 
      xlab = "x1", ylab = "x2", zlab = "f(x1,x2)", 
      main = "Superficie de f(x1, x2)")

# Definimos la función para optimización
f_trig_optim <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  x2 * sin(x1)
}

# Realizamos la optimización
result3 <- optim(c(0, 1), f_trig_optim, method = "BFGS")
print(result3)
