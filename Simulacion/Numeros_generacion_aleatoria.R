# Genera 5 números aleatorios diferentes del 1 al 10
sample(1:10, 5) 

# Selecciona 15 números del 1 al 10 con reemplazo (pueden repetirse)
c <- sample(1:10, 15, replace = TRUE)

# Usa una semilla para obtener los mismos resultados cada vez
set.seed(123)  
c <- sample(1:10, 15, replace = TRUE)

# Imprimir resultados
print("Números aleatorios generados:")
print(c)
