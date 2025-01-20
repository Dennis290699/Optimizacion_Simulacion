# Instalar y cargar el paquete lpSolve
if (!require(lpSolve)) {
  install.packages("lpSolve")
  library(lpSolve)
}

# Definir la función objetivo (costos de transporte)
objective <- c(
  1, 2, 3, 2,  # Costos desde Huerto 1 a Minoristas 1-4
  2, 4, 1, 2,  # Costos desde Huerto 2 a Minoristas 1-4
  1, 3, 5, 3   # Costos desde Huerto 3 a Minoristas 1-4
)

# Definir las restricciones (suministros de los huertos y demandas de los minoristas)
constraints <- matrix(c(
  # Huerto 1
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
  # Huerto 2
  0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
  # Huerto 3
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
  # Minorista 1
  1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
  # Minorista 2
  0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
  # Minorista 3
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  # Minorista 4
  0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1
), nrow = 7, byrow = TRUE)

# Lado derecho de las restricciones (suministros y demandas)
capacities_demands <- c(350, 400, 250, 150, 150, 400, 100)

# Direcciones de las restricciones
directions <- c("<=", "<=", "<=", "=", "=", "=", "=")

# Resolver el problema de programación lineal
solution <- lp("min", objective, constraints, directions, capacities_demands)

# Mostrar el costo mínimo
cat("Costo mínimo de transporte: $", solution$objval, "\n")

# Mostrar la cantidad óptima de cajas a transportar
cat("Distribución óptima de cajas:\n")
for (i in 1:length(solution$solution)) {
  huerto <- ceiling(i / 4)
  minorista <- ((i - 1) %% 4) + 1
  cat("De Huerto", huerto, "a Minorista", minorista, ":", solution$solution[i], "cajas\n")
}

# Crear la matriz de resultados (cajas enviadas de cada huerto a cada minorista)
result_matrix <- matrix(solution$solution, nrow = 3, ncol = 4, byrow = TRUE)

# Agregar nombres a las filas y columnas para mejor presentación
rownames(result_matrix) <- c("Huerto 1", "Huerto 2", "Huerto 3")
colnames(result_matrix) <- c("Minorista 1", "Minorista 2", "Minorista 3", "Minorista 4")

# Mostrar la matriz de resultados
cat("\nMatriz de cajas enviadas de los huertos a los minoristas:\n")
print(result_matrix)
