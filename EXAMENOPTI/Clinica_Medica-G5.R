# Una clínica médica abre sus puertas desde las 7:00 AM hasta las 3:00 PM y atiende a 250 pacientes durante el día.

# Tiempos de llegada de los pacientes
# Los tiempos de llegada siguen una distribución normal con media 0 y desviación estándar 1.
# Para garantizar que las llegadas ocurran dentro del horario de atención, los valores se escalan y ajustan al rango adecuado.

# Asignación de consultorios
# Consultorio 1: 25% de probabilidad, tiempo de consulta con distribución exponencial con media de 6 minutos.
# Consultorio 2: 35% de probabilidad, tiempo de consulta con distribución uniforme entre 4 y 8 minutos.
# Consultorio 3: 40% de probabilidad, tiempo de consulta con distribución Poisson con media de 5 minutos.

# Objetivo
# Simular la atención de los 250 pacientes.
# Determinar cuál es la hora más concurrida del día, es decir, en qué momento hay más pacientes en la clínica.
# Generar gráficos que muestren:
# El tiempo total que cada paciente pasa en la clínica desde su llegada hasta su salida.
# La concurrencia de pacientes en la clínica a lo largo del día.

# Fijar semilla para reproducibilidad
set.seed(123)

# Parámetros
n_pacientes <- 250
hora_apertura <- 7 * 60  # 7:00 AM en minutos
hora_cierre <- 15 * 60   # 3:00 PM en minutos
duracion_dia <- hora_cierre - hora_apertura

# Generar tiempos de llegada (normalizados dentro del rango de atención)
llegadas_normales <- abs(rnorm(n_pacientes, mean = 0, sd = 1))
llegadas_ajustadas <- sort(hora_apertura + (llegadas_normales / max(llegadas_normales)) * duracion_dia)

# Asignación de consultorios con diferentes probabilidades
tipos_consultorio <- sample(1:3, n_pacientes, replace = TRUE, prob = c(0.25, 0.35, 0.40))

# Generar tiempos de consulta según el consultorio asignado
tiempos_consulta <- numeric(n_pacientes)
tiempos_consulta[tipos_consultorio == 1] <- rexp(sum(tipos_consultorio == 1), rate = 1/6)
tiempos_consulta[tipos_consultorio == 2] <- runif(sum(tipos_consultorio == 2), min = 4, max = 8)
tiempos_consulta[tipos_consultorio == 3] <- rpois(sum(tipos_consultorio == 3), lambda = 5)

# Simulación de atención en los consultorios
tiempos_inicio <- numeric(n_pacientes)
tiempos_fin <- numeric(n_pacientes)
tiempo_consultorio_libre <- rep(hora_apertura, 3)

for (i in 1:n_pacientes) {
  consultorio <- tipos_consultorio[i]
  tiempos_inicio[i] <- max(llegadas_ajustadas[i], tiempo_consultorio_libre[consultorio])
  tiempos_fin[i] <- tiempos_inicio[i] + tiempos_consulta[i]
  tiempo_consultorio_libre[consultorio] <- tiempos_fin[i]
}

# Cálculo del tiempo total en el sistema
time_in_system <- tiempos_fin - llegadas_ajustadas

# Crear dataframe con los datos
df_pacientes <- data.frame(
  Paciente = 1:n_pacientes,
  Llegada = llegadas_ajustadas,
  Inicio_Atencion = tiempos_inicio,
  Fin_Atencion = tiempos_fin,
  Tiempo_Sistema = time_in_system
)

# Mostrar primeros y últimos 10 pacientes
print(head(df_pacientes, 10))
print(tail(df_pacientes, 10))

# Determinar concurrencia en la clínica
tiempos_eventos <- sort(c(llegadas_ajustadas, tiempos_fin))
clientes_en_clinica <- numeric(length(tiempos_eventos))
contador <- 0

for (i in 1:length(tiempos_eventos)) {
  contador <- sum(llegadas_ajustadas <= tiempos_eventos[i]) - sum(tiempos_fin <= tiempos_eventos[i])
  clientes_en_clinica[i] <- contador
}

# Encontrar la hora más concurrida
hora_pico <- tiempos_eventos[which.max(clientes_en_clinica)]

# Función para convertir minutos a formato HH:MM
convertir_hora <- function(minutos) {
  hora <- floor(minutos / 60)
  minuto <- round(minutos %% 60)
  sprintf("%02d:%02d", hora, minuto)
}

# Imprimir la hora más concurrida
cat("La hora más concurrida del día es a las", convertir_hora(hora_pico), "\n")

# Graficar tiempo total en el sistema por paciente
library(ggplot2)
df_tiempo_sistema <- data.frame(Paciente = 1:n_pacientes, Tiempo = time_in_system)
ggplot(df_tiempo_sistema, aes(x = Paciente, y = Tiempo)) +
  geom_point(color = "blue") +
  geom_line(color = "orange") +
  labs(title = "Tiempo Total en el Sistema por Paciente",
       x = "Paciente",
       y = "Tiempo en el Sistema (min)") +
  theme_minimal()

# Graficar concurrencia de pacientes en la clínica
df_concurrencia <- data.frame(Tiempo = tiempos_eventos, Pacientes = clientes_en_clinica)
ggplot(df_concurrencia, aes(x = Tiempo, y = Pacientes)) +
  geom_line(color = "green") +
  scale_x_continuous(breaks = seq(hora_apertura, hora_cierre, by = 60),
                     labels = sapply(seq(hora_apertura, hora_cierre, by = 60), convertir_hora)) +
  labs(title = "Concurrencia de Pacientes en la Clínica",
       x = "Hora del Día",
       y = "Pacientes en la Clínica") +
  theme_minimal()
