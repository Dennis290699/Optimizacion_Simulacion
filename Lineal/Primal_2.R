# Instalar y cargar el paquete lpSolve si no está instalado
if (!require(lpSolve)) install.packages("lpSolve", dependencies = TRUE)
library(lpSolve)

# Coeficientes de la función objetivo (Maximizar Z = 20x1 + 50x2)
objective <- c(20, 50)

# Restricciones
# Restricción 1: x1 >= 4x2 ->  x1 - 4x2 >= 0
constraint1 <- c(1, -4)
# Restricción 2: x1 <= 100 ->  x1 <= 100
constraint2 <- c(1, 0)
# Restricción 3: 2x1 + 4x2 <= 240 ->  2x1 + 4x2 <= 240
constraint3 <- c(2, 4)

# Lado derecho de las restricciones
rhs <- c(0, 100, 240)

# Tipo de las restricciones
directions <- c(">=", "<=", "<=")

# Resolver el problema de programación lineal (Maximizar)
solution_primal <- lp(direction = "max",  # Maximización
                      objective.in = objective, 
                      const.mat = rbind(constraint1, constraint2, constraint3), 
                      const.dir = directions, 
                      const.rhs = rhs)

# Obtener la solución
x1_opt <- solution_primal$solution[1]  # Valor óptimo de x1
x2_opt <- solution_primal$solution[2]  # Valor óptimo de x2
z_opt <- solution_primal$objval        # Valor máximo de la función objetivo Z

# Mostrar los resultados
if (solution_primal$status == 0) {
  cat("El modelo primal fue resuelto con éxito.\n")
  cat("Valor máximo de la función objetivo (Z):", z_opt, "\n")
  cat("Valores óptimos:\n")
  cat("x1:", x1_opt, "\n")
  cat("x2:", x2_opt, "\n")
} else {
  cat("El modelo primal no tiene solución.\n")
}
