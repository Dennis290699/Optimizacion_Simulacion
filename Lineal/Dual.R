# Instalar y cargar paquetes necesarios
if (!require(lpSolve)) install.packages("lpSolve", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(lpSolve)
library(ggplot2)

# Definir los coeficientes de la función objetivo (Dual: minimizar)
objective_dual <- c(30, 10, 10)  # Coeficientes asociados a y1, y2, y3

# Definir la matriz de restricciones (Dual: desigualdades >=)
constraints_dual <- matrix(c(
  1, 1, 0,  # Restricción de y1 + y2 >= 1500
  1, 0, 1   # Restricción de y1 + y3 >= 1000
), nrow = 2, byrow = TRUE)

# Lado derecho de las restricciones
rhs_dual <- c(1500, 1000)

# Tipos de restricciones (>=)
directions_dual <- c(">=", ">=")

# Resolver el problema de programación lineal dual
solution_dual <- lp(direction = "min",  # Minimización
                    objective.in = objective_dual, 
                    const.mat = constraints_dual, 
                    const.dir = directions_dual, 
                    const.rhs = rhs_dual)

# Obtener la solución
y1_opt <- solution_dual$solution[1]  # Valor óptimo de y1
y2_opt <- solution_dual$solution[2]  # Valor óptimo de y2
y3_opt <- solution_dual$solution[3]  # Valor óptimo de y3
w_opt <- solution_dual$objval        # Valor mínimo de la función objetivo (W)

# Mostrar los resultados
if (solution_dual$status == 0) {
  cat("El modelo dual fue resuelto con éxito.\n")
  cat("Valor mínimo de la función objetivo (W):", w_opt, "\n")
  cat("Valores óptimos:\n")
  cat("y1:", y1_opt, "\n")
  cat("y2:", y2_opt, "\n")
  cat("y3:", y3_opt, "\n")
} else {
  cat("El modelo dual no tiene solución.\n")
}

# Crear puntos para las restricciones y la región factible
y1 <- seq(0, 3000, length.out = 100)  # Valores de y1 para graficar

# Restricciones duales:
line1 <- 1500 - y1  # y1 + y2 >= 1500 -> y2 = 1500 - y1
line2 <- 1000 - y1  # y1 + y3 >= 1000 -> y3 = 1000 - y1

# Limitar las líneas al rango positivo
line1[line1 < 0] <- NA
line2[line2 < 0] <- NA

# Crear un data frame para las líneas
data_dual <- data.frame(
  y1 = c(y1, y1),
  y = c(line1, line2),
  group = c(rep("y1 + y2 ≥ 1500", length(y1)),
            rep("y1 + y3 ≥ 1000", length(y1)))
)

# Graficar la región factible del dual
ggplot(data_dual, aes(x = y1, y = y, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(data = subset(data_dual, group == "y1 + y2 ≥ 1500"), 
              aes(ymin = 0, ymax = pmax(0, 1500 - y1)), fill = "blue", alpha = 0.2) +
  geom_ribbon(data = subset(data_dual, group == "y1 + y3 ≥ 1000"), 
              aes(ymin = 0, ymax = pmax(0, 1000 - y1)), fill = "green", alpha = 0.2) +
  annotate("point", x = y1_opt, y = y2_opt, color = "red", size = 3) +
  annotate("text", x = y1_opt + 50, y = y2_opt, 
           label = paste("Óptimo:\n(", round(y1_opt, 2), ", ", round(y2_opt, 2), ")", sep = ""), 
           color = "red") +
  labs(title = "Región Factible del Problema Dual y Solución Óptima",
       x = "y1",
       y = "y2 / y3",
       color = "Restricciones") +
  theme_minimal()
