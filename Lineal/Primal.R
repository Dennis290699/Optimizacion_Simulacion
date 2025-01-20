# Instalar y cargar paquetes necesarios
if (!require(lpSolve)) install.packages("lpSolve", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)

library(lpSolve)
library(ggplot2)

# Definir los coeficientes de la función objetivo (ingresos por curso)
objective <- c(1500, 1000)

# Definir la matriz de restricciones (coeficientes de las restricciones)
constraints <- matrix(c(
  1, 1,  # Restricción: x1 + x2 <= 30
  -1, 0, # Restricción: x1 >= 10 (convertida a -x1 <= -10)
  0, -1  # Restricción: x2 >= 10 (convertida a -x2 <= -10)
), nrow = 3, byrow = TRUE)

# Lado derecho de las restricciones
rhs <- c(30, -10, -10)

# Tipos de restricciones (<=)
directions <- c("<=", "<=", "<=")

# Resolver el problema de programación lineal
solution <- lp(direction = "max",  # Maximización
               objective.in = objective, 
               const.mat = constraints, 
               const.dir = directions, 
               const.rhs = rhs)

# Obtener la solución
x1_opt <- solution$solution[1]  # Cursos prácticos
x2_opt <- solution$solution[2]  # Cursos humanísticos
z_opt <- solution$objval        # Valor máximo de la función objetivo

# Mostrar los resultados
if (solution$status == 0) {
  cat("El modelo fue resuelto con éxito.\n")
  cat("Valor máximo de la función objetivo (ingresos totales):", solution$objval, "\n")
  cat("Cursos prácticos (x1):", solution$solution[1], "\n")
  cat("Cursos humanísticos (x2):", solution$solution[2], "\n")
} else {
  cat("El modelo no tiene solución.\n")
}

# Crear datos para las restricciones y la región factible
x <- seq(0, 30, length.out = 100)  # Valores de x1 para graficar

# Restricciones:
line1 <- 30 - x  # x1 + x2 <= 30 -> x2 = 30 - x1
line2 <- rep(10, length(x))  # x1 >= 10 -> Línea constante
line3 <- rep(10, length(x))  # x2 >= 10 -> Línea constante

# Crear un data frame para las líneas
data <- data.frame(
  x = c(x, x, x),
  y = c(line1, line2, line3),
  group = c(rep("x1 + x2 ≤ 30", length(x)),
            rep("x1 ≥ 10", length(x)),
            rep("x2 ≥ 10", length(x)))
)

# Graficar la región factible y el punto óptimo correctamente
ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(data = subset(data, group == "x1 + x2 ≤ 30"), aes(ymin = 10, ymax = pmin(30 - x, 30)), fill = "blue", alpha = 0.2) +
  geom_point(data = data.frame(x = x1_opt, y = x2_opt), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text", x = x1_opt, y = x2_opt + 1, label = paste("Óptimo:\n(", x1_opt, ", ", x2_opt, ")", sep = ""), color = "red") +
  labs(title = "Región Factible y Solución Óptima",
       x = "Cursos Prácticos (x1)",
       y = "Cursos Humanísticos (x2)",
       color = "Restricciones") +
  theme_minimal()
