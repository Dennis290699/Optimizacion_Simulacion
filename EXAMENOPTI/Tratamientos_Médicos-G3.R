set.seed(123)  # Semilla para reproducibilidad

# Parámetros del ejercicio
n_pacientes <- 500
tratamientos <- c("A", "B")  # Tratamientos A y B

# 1. Asignación aleatoria de tratamientos
grupo_tratamiento <- sample(tratamientos, n_pacientes, replace = TRUE)

# 2. Generación de los tiempos de recuperación
# Tratamiento A: Normal con media 15 días y desviación estándar 3 días
# Tratamiento B: Normal con media 20 días y desviación estándar 4 días
tiempos_recuperacion <- numeric(n_pacientes)

for (i in 1:n_pacientes) {
  if (grupo_tratamiento[i] == "A") {
    tiempos_recuperacion[i] <- rnorm(1, mean = 15, sd = 3)
  } else {
    tiempos_recuperacion[i] <- rnorm(1, mean = 20, sd = 4)
  }
}

# 3. Determinar cuántos pacientes de cada grupo se recuperan en menos de 18 días
pacientes_recuperados_A <- sum(grupo_tratamiento == "A" & tiempos_recuperacion < 18)
pacientes_recuperados_B <- sum(grupo_tratamiento == "B" & tiempos_recuperacion < 18)

# 4. Calcular la media de recuperación para cada tratamiento
media_recuperacion_A <- mean(tiempos_recuperacion[grupo_tratamiento == "A"])
media_recuperacion_B <- mean(tiempos_recuperacion[grupo_tratamiento == "B"])

# Crear un data frame con los resultados
resultado <- data.frame(
  Paciente = 1:n_pacientes,
  Tratamiento = grupo_tratamiento,
  Tiempo_Recuperacion = tiempos_recuperacion
)

# 5. Mostrar los resultados
cat("Número de pacientes de Tratamiento A que se recuperan en menos de 18 días:", pacientes_recuperados_A, "\n")
cat("Número de pacientes de Tratamiento B que se recuperan en menos de 18 días:", pacientes_recuperados_B, "\n")
cat("Media de recuperación para Tratamiento A:", round(media_recuperacion_A, 2), "días\n")
cat("Media de recuperación para Tratamiento B:", round(media_recuperacion_B, 2), "días\n")

# 6. Comparar los tratamientos visualmente
library(ggplot2)

# Gráfico de los tiempos de recuperación para cada tratamiento
ggplot(resultado, aes(x = Tiempo_Recuperacion, fill = Tratamiento)) +
  geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de los Tiempos de Recuperación por Tratamiento",
       x = "Tiempo de Recuperación (días)", y = "Número de Pacientes") +
  scale_fill_manual(values = c("A" = "lightblue", "B" = "lightgreen")) +
  theme_minimal()

# Gráfico de densidad para comparar los tiempos de recuperación entre los tratamientos
ggplot(resultado, aes(x = Tiempo_Recuperacion, fill = Tratamiento)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribución de los Tiempos de Recuperación por Tratamiento",
       x = "Tiempo de Recuperación (días)", y = "Densidad") +
  scale_fill_manual(values = c("A" = "lightblue", "B" = "lightgreen")) +
  theme_minimal()

