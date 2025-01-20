# Instalar y cargar el paquete lpSolve
if (!require(lpSolve)) {
  install.packages("lpSolve")
  library(lpSolve)
}

# Definir la función objetivo (costos de transporte)
objective <- c(2, 3,  # Costos desde Fábrica A
               1, 4,  # Costos desde Fábrica B
               3, 2)  # Costos desde Fábrica C

# Definir las restricciones (capacidades de las fábricas y demandas de los almacenes)
constraints <- matrix(c(1, 1, 0, 0, 0, 0,  # Fábrica A
                        0, 0, 1, 1, 0, 0,  # Fábrica B
                        0, 0, 0, 0, 1, 1,  # Fábrica C
                        1, 0, 1, 0, 1, 0,  # Almacén 1
                        0, 1, 0, 1, 0, 1), # Almacén 2
                      nrow = 5, byrow = TRUE)

# Definir el lado derecho de las restricciones (capacidades y demandas)
capacities_demands <- c(50, 70, 40, 60, 50)

# Definir las direcciones de las restricciones
directions <- c("<=", "<=", "<=", ">=", ">=")

# Resolver el problema de programación lineal
solution <- lp("min", objective, constraints, directions, capacities_demands)

# Mostrar el costo mínimo
cat("Costo mínimo de transporte: $", solution$objval, "\n")

# Mostrar la cantidad óptima de productos a transportar
cat("Cantidad óptima de productos a transportar:\n")
for (i in 1:length(solution$solution)) {
  cat("De Fábrica", ceiling(i/2), "a Almacén", ((i-1) %% 2) + 1, ":", solution$solution[i], "mesas\n")
}

# Crear la matriz de resultados (productos enviados de cada fábrica a cada almacén)
result_matrix <- matrix(solution$solution, nrow = 3, ncol = 2, byrow = TRUE)

# Agregar nombres a las filas y columnas para mejor presentación
rownames(result_matrix) <- c("Fábrica A", "Fábrica B", "Fábrica C")
colnames(result_matrix) <- c("Almacén 1", "Almacén 2")

# Mostrar la matriz de resultados
cat("\nMatriz de productos enviados de las fábricas a los almacenes:\n")
print(result_matrix)
