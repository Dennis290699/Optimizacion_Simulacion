# Cargar la librería
if (!require(lpSolve)) install.packages("lpSolve")
library(lpSolve)

# Costos de transporte
costos <- matrix(c(2, 3, 1, 
                   5, 4, 2, 
                   3, 2, 4), 
                 nrow = 3, byrow = TRUE)

# Suministros de los almacenes
suministro <- c(100, 200, 150)

# Demandas de los destinos
demanda <- c(150, 200, 100)

# Resolver el problema de transporte
resultado <- lp.transport(costos, direction = "min", 
                          row.signs = rep("=", length(suministro)), 
                          row.rhs = suministro, 
                          col.signs = rep("=", length(demanda)), 
                          col.rhs = demanda)

# Imprimir el costo total
cat("Costo total mínimo:", resultado$objval, "\n")

# Imprimir el plan óptimo de transporte
cat("Plan óptimo de transporte:\n")
matrix(resultado$solution, nrow = 3, byrow = TRUE)
