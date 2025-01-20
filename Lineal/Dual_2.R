# Instalar y cargar el paquete lpSolve si no está instalado
if (!require(lpSolve)) install.packages("lpSolve", dependencies = TRUE)
library(lpSolve)

# Coeficientes de la función objetivo dual (Minimizar W = 100y1 + 240y2 + 0y3)
objective_dual <- c(100, 240, 0)

# Restricciones del dual
# Restricción 1: y1 + 2y2 >= 20
constraint1_dual <- c(1, 2, 0)
# Restricción 2: -4y1 + 4y2 >= 50
constraint2_dual <- c(-4, 4, 0)

# Lado derecho de las restricciones del dual
rhs_dual <- c(20, 50)

# Tipo de las restricciones
directions_dual <- c(">=", ">=")

# Resolver el problema de programación lineal dual (Minimización)
solution_dual <- lp(direction = "min",  # Minimización
                     objective.in = objective_dual, 
                     const.mat = rbind(constraint1_dual, constraint2_dual), 
                     const.dir = directions_dual, 
                     const.rhs = rhs_dual)

# Obtener la solución dual
y1_opt <- solution_dual$solution[1]  # Valor óptimo de y1
y2_opt <- solution_dual$solution[2]  # Valor óptimo de y2
y3_opt <- solution_dual$solution[3]  # Valor óptimo de y3 (aunque no afecta la función objetivo)
w_opt <- solution_dual$objval        # Valor mínimo de la función objetivo (W)

# Mostrar los resultados
if (solution_dual$status == 0) {
  cat("El modelo dual fue resuelto con éxito.\n")
  cat("Valor mínimo de la función objetivo (W):", w_opt, "\n")
  cat("Valores óptimos de las variables duales:\n")
  cat("y1:", y1_opt, "\n")
  cat("y2:", y2_opt, "\n")
  cat("y3:", y3_opt, "\n")
} else {
  cat("El modelo dual no tiene solución.\n")
}
