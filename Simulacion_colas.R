set.seed(123)  # Fijar semilla para reproducibilidad

# Parámetros de la simulación
n_clientes <- 10  # Número de clientes a simular
tasa_llegada <- 1/5  # Media de llegada de clientes (5 min)
tasa_servicio <- 1/7  # Media de atención (7 min)

# Generar tiempos de llegada exponenciales
tiempo_llegada <- cumsum(rexp(n_clientes, rate = tasa_llegada))

# Generar tiempos de servicio exponenciales
tiempo_servicio <- rexp(n_clientes, rate = tasa_servicio)

# Inicializar vectores para tiempos de inicio y fin
tiempo_inicio <- numeric(n_clientes)
tiempo_fin <- numeric(n_clientes)

# Simulación de la atención en el cajero
for (i in 1:n_clientes) {
  if (i == 1) {
    tiempo_inicio[i] <- tiempo_llegada[i]  # Primer cliente es atendido de inmediato
  } else {
    tiempo_inicio[i] <- max(tiempo_llegada[i], tiempo_fin[i-1])  # Espera si el cajero está ocupado
  }
  tiempo_fin[i] <- tiempo_inicio[i] + tiempo_servicio[i]  # Fin del servicio
}

# Calcular métricas
tiempo_espera <- tiempo_inicio - tiempo_llegada  # Tiempo que esperó en la cola
tiempo_total <- tiempo_fin - tiempo_llegada  # Tiempo total en el sistema

# Crear un data frame con los resultados
resultado <- data.frame(
  Cliente = 1:n_clientes,
  Llegada = round(tiempo_llegada, 2),
  Inicio_Atencion = round(tiempo_inicio, 2),
  Fin_Atencion = round(tiempo_fin, 2),
  Tiempo_Espera = round(tiempo_espera, 2),
  Tiempo_Total = round(tiempo_total, 2)
)

# Mostrar resultados
print(resultado)

# Graficar el comportamiento de la cola
library(ggplot2)
ggplot(resultado, aes(x = Cliente)) +
  geom_point(aes(y = Tiempo_Espera), color = "red", size = 3) +
  geom_line(aes(y = Tiempo_Espera), color = "red") +
  geom_point(aes(y = Tiempo_Total), color = "blue", size = 3) +
  geom_line(aes(y = Tiempo_Total), color = "blue") +
  labs(title = "Tiempo de Espera y Tiempo Total en el Sistema",
       x = "Cliente", y = "Tiempo (min)") +
  scale_y_continuous(limits = c(0, max(tiempo_total))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 2, y = max(tiempo_total) * 0.9, 
           label = "Rojo: Espera, Azul: Total", color = "black", size = 5)
set.seed(123)  # Fijar semilla para reproducibilidad

# Parámetros de la simulación
n_clientes <- 10  # Número de clientes a simular
tasa_llegada <- 1/5  # Media de llegada de clientes (5 min)
tasa_servicio <- 1/7  # Media de atención (7 min)

# Generar tiempos de llegada exponenciales
tiempo_llegada <- cumsum(rexp(n_clientes, rate = tasa_llegada))

# Generar tiempos de servicio exponenciales
tiempo_servicio <- rexp(n_clientes, rate = tasa_servicio)

# Inicializar vectores para tiempos de inicio y fin
tiempo_inicio <- numeric(n_clientes)
tiempo_fin <- numeric(n_clientes)

# Simulación de la atención en el cajero
for (i in 1:n_clientes) {
  if (i == 1) {
    tiempo_inicio[i] <- tiempo_llegada[i]  # Primer cliente es atendido de inmediato
  } else {
    tiempo_inicio[i] <- max(tiempo_llegada[i], tiempo_fin[i-1])  # Espera si el cajero está ocupado
  }
  tiempo_fin[i] <- tiempo_inicio[i] + tiempo_servicio[i]  # Fin del servicio
}

# Calcular métricas
tiempo_espera <- tiempo_inicio - tiempo_llegada  # Tiempo que esperó en la cola
tiempo_total <- tiempo_fin - tiempo_llegada  # Tiempo total en el sistema

# Crear un data frame con los resultados
resultado <- data.frame(
  Cliente = 1:n_clientes,
  Llegada = round(tiempo_llegada, 2),
  Inicio_Atencion = round(tiempo_inicio, 2),
  Fin_Atencion = round(tiempo_fin, 2),
  Tiempo_Espera = round(tiempo_espera, 2),
  Tiempo_Total = round(tiempo_total, 2)
)

# Mostrar resultados
print(resultado)

# Graficar el comportamiento de la cola
library(ggplot2)
ggplot(resultado, aes(x = Cliente)) +
  geom_point(aes(y = Tiempo_Espera), color = "red", size = 3) +
  geom_line(aes(y = Tiempo_Espera), color = "red") +
  geom_point(aes(y = Tiempo_Total), color = "blue", size = 3) +
  geom_line(aes(y = Tiempo_Total), color = "blue") +
  labs(title = "Tiempo de Espera y Tiempo Total en el Sistema",
       x = "Cliente", y = "Tiempo (min)") +
  scale_y_continuous(limits = c(0, max(tiempo_total))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 2, y = max(tiempo_total) * 0.9, 
           label = "Rojo: Espera, Azul: Total", color = "black", size = 5)
           