set.seed(123)  # Fijar semilla para reproducibilidad

n_clientes <- 5           # Número de clientes a simular
lambda <- 1/15            # Tasa de llegada (1/media de 15 minutos)

# Para el primer cliente el tiempo entre llegadas es 0, 
# y para los siguientes se generan tiempos exponenciales.
tiempo_entre_llegadas <- c(0, rexp(n_clientes - 1, rate = lambda))
tiempo_llegada <- cumsum(tiempo_entre_llegadas)

# Generar tiempos de servicio (distribución uniforme entre 10 y 15 minutos)
tiempo_servicio <- runif(n_clientes, min = 10, max = 15)

# Inicializar vectores para almacenar los resultados
inicio_servicio <- numeric(n_clientes)
tiempo_espera   <- numeric(n_clientes)
tiempo_salida   <- numeric(n_clientes)

# Simulación de la atención (FIFO)
for (i in 1:n_clientes) {
  if (i == 1) {
    inicio_servicio[i] <- tiempo_llegada[i]
  } else {
    inicio_servicio[i] <- max(tiempo_llegada[i], tiempo_salida[i - 1])
  }
  tiempo_espera[i] <- max(0, inicio_servicio[i] - tiempo_llegada[i])
  tiempo_salida[i] <- inicio_servicio[i] + tiempo_servicio[i]
}

# Definir el estado de la instalación:
# Se considera que se forma cola si el cliente tuvo que esperar.
estado_instalacion <- ifelse(tiempo_espera > 0, "Con espera", "Sin espera")

# Crear un DataFrame con los resultados relevantes
tabla_resultados <- data.frame(
  Cliente               = 1:n_clientes,
  Tiempo_Llegada        = round(tiempo_llegada, 2),
  Tiempo_Entre_Llegadas = round(tiempo_entre_llegadas, 2),
  Tiempo_Servicio       = round(tiempo_servicio, 2),
  Tiempo_Espera         = round(tiempo_espera, 2),
  Tiempo_Salida         = round(tiempo_salida, 2),
  Estado_Instalacion    = estado_instalacion
)

print(tabla_resultados)

# Cálculo de métricas de desempeño

# 1. Utilización promedio de la peluquería: 
#    razón entre el tiempo total de servicio y el tiempo total de simulación (hasta la última salida).
utilizacion_peluqueria <- sum(tiempo_servicio) / max(tiempo_salida)

# 2. Cantidad promedio de clientes que esperan (número promedio en cola):
#    Se calcula dividiendo el área total de espera (suma de los tiempos de espera)
#    entre el tiempo total de simulación.
cantidad_promedio_cola <- sum(tiempo_espera) / max(tiempo_salida)

# 3. Tiempo promedio que un cliente espera en la cola.
tiempo_promedio_espera <- mean(tiempo_espera)

cat("\nMedidas de desempeño:\n")
cat("1. Utilización promedio de la peluquería:", round(utilizacion_peluqueria, 2), "\n")
cat("2. Cantidad promedio de clientes que esperan:", round(cantidad_promedio_cola, 2), "\n")
cat("3. Tiempo promedio que un cliente espera en la cola:", round(tiempo_promedio_espera, 2), "min\n")