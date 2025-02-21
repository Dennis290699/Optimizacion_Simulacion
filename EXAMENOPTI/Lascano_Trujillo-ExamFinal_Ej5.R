set.seed(123)

# Datos
numero_pacientes <- 250 
hora_inicio <- 7 * 60  #Hora -> Minutos
hora_fin <- 15 * 60   #Hora -> Minutos
duracion_trabajo <- hora_fin - hora_inicio

# Tiempos de llegada de los pacientes
llegadas_sinrest <- abs(rnorm(numero_pacientes, mean = 0, sd = 1))
# Ordenamiento ajustado y escalado
llegadas_condicion <- sort(hora_inicio + (llegadas_sinrest / max(llegadas_sinrest)) * duracion_trabajo)

# Tipos de consultorios
tipos_consultorio <- sample(1:3, numero_pacientes, replace = TRUE, prob = c(0.25, 0.35, 0.40))

# De minutos a horas
conversor_hora <- function(minutos) {
  hora <- floor(minutos/60)
  minuto <- round(minutos%%60)
  sprintf("%02d:%02d", hora, minuto)
}


tiempo_consulta <- numeric(numero_pacientes)
tiempo_consulta[tipos_consultorio == 1] <- rexp(sum(tipos_consultorio == 1), rate = 1/6)
tiempo_consulta[tipos_consultorio == 2] <- runif(sum(tipos_consultorio == 2), min = 4, max = 8)
tiempo_consulta[tipos_consultorio == 3] <- rpois(sum(tipos_consultorio == 3), lambda = 5)

hora_llegadaPaciente <- numeric(numero_pacientes)
hora_salidaPaciente <- numeric(numero_pacientes)
consultorio_libre <- rep(hora_inicio, 3)

for (i in 1:numero_pacientes) {
  consultorio <- tipos_consultorio[i]
  hora_llegadaPaciente[i] <- max(llegadas_condicion[i], consultorio_libre[consultorio])
  hora_salidaPaciente[i] <- hora_llegadaPaciente[i] + tiempo_consulta[i]
  consultorio_libre[consultorio] <- hora_salidaPaciente[i]
}

# Crear la tabla de resultados
resultados <- data.frame(
  num_paciente = 1 : numero_pacientes, 
  tiempo_consulta = tiempo_consulta,
  inicio_consulta = conversor_hora(hora_llegadaPaciente),
  fin_consulta = conversor_hora(hora_salidaPaciente),
  llegadas = conversor_hora(llegadas_condicion)
)

print(resultados)

tiempos_eventos <- sort(c(llegadas_condicion, hora_salidaPaciente))
clientes_clinica <- numeric(length(tiempos_eventos))
contador <- 0

for (i in 1 : length(tiempos_eventos)) {
  contador <- sum(llegadas_condicion <= tiempos_eventos[i])- sum(hora_salidaPaciente <= tiempos_eventos[i]) 
  clientes_clinica[i] <- contador
}

hora_pico <- tiempos_eventos[which.max(clientes_clinica)]

cat("la hora mas concurrida es:", conversor_hora(hora_pico),"\n")

# Graficas

library(ggplot2)
df_tiempo_sistema <- data.frame(Paciente = 1:numero_pacientes, Tiempo = tiempo_consulta)
ggplot(df_tiempo_sistema, aes(x = Paciente, y = Tiempo)) +
  geom_point(color = "red") +
  geom_line(color = "green") +
  labs(title = "Tiempo Total en el Consultorio por Paciente",
       x = "Paciente",
       y = "Tiempo en el Consultorio (min)") +
  theme_minimal()

# Graficar concurrencia de pacientes en la clínica
df_concurrencia <- data.frame(Tiempo = tiempos_eventos, Pacientes = clientes_clinica)
ggplot(df_concurrencia, aes(x = Tiempo, y = Pacientes)) +
  geom_line(color = "blue") +
  scale_x_continuous(breaks = seq(hora_inicio, hora_fin, by = 60),
                     labels = sapply(seq(hora_inicio, hora_fin, by = 60), conversor_hora)) +
  labs(title = "Concurrencia de Pacientes en la Clínica",
       x = "Hora del Día",
       y = "Pacientes en la Clínica") +
  theme_minimal()
