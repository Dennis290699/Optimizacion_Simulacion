# Simulación de atención de 300 clientes en un banco, Horario: 8:00 AM a 4:00 PM

set.seed(123)  # Semilla

# Parámetros del ejercicio
n_clientes    <- 300
horas_apertura <- 8    # 8:00 AM
horas_cierre   <- 16   # 4:00 PM
duracion_dia   <- horas_cierre - horas_apertura  # Duración en horas

# 1. Tiempos de llegada de los clientes
# Se generan valores con distribución normal (usando valores absolutos para que sean positivos)
llegadas_normales   <- abs(rnorm(n_clientes, mean = 0, sd = 1))
# Se escalan los tiempos para que queden entre 8:00 y 16:00 y se ordenan cronológicamente
llegadas_ajustadas  <- sort(horas_apertura + (llegadas_normales / max(llegadas_normales)) * duracion_dia)

# 2. Asignación de cajas y tiempos de trámite
# Probabilidades para cada caja: Caja 1 (20%), Caja 2 (30%), Caja 3 (50%)
cajas <- sample(c(1, 2, 3), n_clientes, replace = TRUE, prob = c(0.2, 0.3, 0.5))

# Generar tiempos de trámite según la caja asignada:
# - Caja 1: Distribución exponencial con media 5 minutos.
# - Caja 2: Distribución uniforme entre 5 y 10 minutos.
# - Caja 3: Distribución Poisson con media 4 minutos.
tiempos_tramite <- numeric(n_clientes)
for (i in 1:n_clientes) {
  if (cajas[i] == 1) {
    tiempos_tramite[i] <- rexp(1, rate = 1/5)
  } else if (cajas[i] == 2) {
    tiempos_tramite[i] <- runif(1, min = 5, max = 10)
  } else {  # Caja 3
    tiempos_tramite[i] <- rpois(1, lambda = 4)
  }
}
# Convertir tiempos de trámite de minutos a horas
tiempos_tramite_horas <- tiempos_tramite / 60

# 3. Simulación de la atención de los clientes
# Se calcula el momento en que inicia y finaliza la atención para cada cliente
tiempo_inicio <- numeric(n_clientes)
tiempo_fin    <- numeric(n_clientes)

for (i in 1:n_clientes) {
  if (i == 1) {
    tiempo_inicio[i] <- llegadas_ajustadas[i]  # El primer cliente es atendido al llegar
  } else {
    # Cada cliente inicia cuando llega o cuando el anterior finalizó, lo que ocurra más tarde
    tiempo_inicio[i] <- max(llegadas_ajustadas[i], tiempo_fin[i - 1])
  }
  tiempo_fin[i] <- tiempo_inicio[i] + tiempos_tramite_horas[i]
}

# Tiempo total que cada cliente pasa en el sistema (desde su llegada hasta que finaliza su trámite)
tiempo_total <- tiempo_fin - llegadas_ajustadas

# Crear un data frame con los resultados
resultado <- data.frame(
  Cliente        = 1:n_clientes,
  Llegada        = round(llegadas_ajustadas, 2),
  Inicio_Atencion= round(tiempo_inicio, 2),
  Fin_Atencion   = round(tiempo_fin, 2),
  Tiempo_Total   = round(tiempo_total, 2),
  Caja           = cajas
)

# Visualizar los primeros resultados
print("Resultados de la simulación:")
print(head(resultado, 10))

# Determinar la hora más concurrida del día
# Se evalúa la concurrencia en intervalos de 0.1 horas (~6 minutos)
horas      <- seq(horas_apertura, horas_cierre, by = 0.1)
concurrencia <- sapply(horas, function(h) sum(llegadas_ajustadas <= h & tiempo_fin > h))
hora_pico  <- horas[which.max(concurrencia)]
print(paste("La hora más concurrida es:", round(hora_pico, 2), "horas"))

# 4. Graficar resultados
library(ggplot2)

# Gráfico del Tiempo Total en el Sistema por Cliente (convertido a minutos)
ggplot(resultado, aes(x = Cliente, y = Tiempo_Total * 60)) +
  geom_point(color = "blue") +
  geom_line(color = "blue") +
  labs(title = "Tiempo Total en el Sistema por Cliente",
       x = "Cliente", y = "Tiempo Total (min)") +
  theme_minimal()

# Gráfico de la concurrencia en el banco a lo largo del día
data_concurrencia <- data.frame(Hora = horas, Concurrencia = concurrencia)

ggplot(data_concurrencia, aes(x = Hora, y = Concurrencia)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Concurrencia en el Banco a lo Largo del Día",
       x = "Hora del Día", y = "Número de Clientes") +
  theme_minimal()
