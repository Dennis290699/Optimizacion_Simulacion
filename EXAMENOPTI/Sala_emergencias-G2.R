# Instalar los paquetes si es necesario
if (!require(simmer)) install.packages("simmer")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")

library(simmer)
library(dplyr)
library(ggplot2)

simulate_hospital <- function(num_doctors = 4, num_stretchers = 6, simulation_time = 600,
                              prioritized = FALSE, arrival_rate = 12/60) {
  env <- simmer("HospitalSimulation")
  
  # Definir la trayectoria del paciente
  patient_traj <- trajectory("Camino del paciente") %>%
    branch(
      option = function() if(prioritized) 1 else 0,
      continue = TRUE,
      trajectory("Asignar Severidad") %>%
        set_attribute("severity", function() if(runif(1) < 0.3) 0 else 1)
    ) %>%
    
    # Evaluación médica: solicitar un médico y asignar tiempo de atención ~ N(20,5)
    seize("doctor", amount = 1,
          priority = if(prioritized) function() get_attribute(env, "severity") else 0) %>%
    timeout(function() max(rnorm(1, mean = 20, sd = 5), 1)) %>%
    release("doctor", amount = 1) %>%
    # Con probabilidad 60%, el paciente requiere camilla para pruebas/tratamiento
    branch(
      option = function() sample(0:1, 1, prob = c(0.4, 0.6)),
      continue = TRUE,
      trajectory("Requiere Camilla") %>%
        seize("stretcher", amount = 1,
              priority = if(prioritized) function() get_attribute(env, "severity") else 0) %>%
        timeout(function() runif(1, min = 15, max = 25)) %>%
        release("stretcher", amount = 1)
    )
  
  env %>%
    add_resource("doctor", capacity = num_doctors, queue_size = Inf) %>%
    add_resource("stretcher", capacity = num_stretchers, queue_size = Inf) %>%
    add_generator("patient", patient_traj, function() rexp(1, rate = arrival_rate))
  
  env %>% run(until = simulation_time)
  return(env)
}

simulate_hospital_nh <- function(num_doctors = 4, num_stretchers = 6, simulation_time = 600,
                                 prioritized = FALSE) {
  env <- simmer("HospitalSimulation_NH")
  
  patient_traj <- trajectory("Camino del paciente") %>%
    branch(
      option = function() if(prioritized) 1 else 0,
      continue = TRUE,
      trajectory("Asignar Severidad") %>%
        set_attribute("severity", function() if(runif(1) < 0.3) 0 else 1)
    ) %>%
    seize("doctor", amount = 1,
          priority = if(prioritized) function() get_attribute(env, "severity") else 0) %>%
    timeout(function() max(rnorm(1, mean = 20, sd = 5), 1)) %>%
    release("doctor", amount = 1) %>%
    branch(
      option = function() sample(0:1, 1, prob = c(0.4, 0.6)),
      continue = TRUE,
      trajectory("Requiere Camilla") %>%
        seize("stretcher", amount = 1,
              priority = if(prioritized) function() get_attribute(env, "severity") else 0) %>%
        timeout(function() runif(1, min = 15, max = 25)) %>%
        release("stretcher", amount = 1)
    )
  
  env %>%
    add_resource("doctor", capacity = num_doctors, queue_size = Inf) %>%
    add_resource("stretcher", capacity = num_stretchers, queue_size = Inf)
  
  # Generar tiempos de llegada 
  # - 0 a 120 min: 12 pacientes/hora (0.2/min)
  # - 120 a 360 min: 15 pacientes/hora (0.25/min)
  # - 360 a 600 min: 12 pacientes/hora (0.2/min)
  arrival_times <- c()
  t <- 0
  while(t < simulation_time) {
    if(t < 120) {
      lambda <- 12/60
    } else if(t >= 120 && t < 360) {
      lambda <- 15/60
    } else {
      lambda <- 12/60
    }
    ia <- rexp(1, rate = lambda)
    t <- t + ia
    if(t < simulation_time) {
      arrival_times <- c(arrival_times, t)
    }
  }

  arrivals_df <- data.frame(name = seq_along(arrival_times), time = arrival_times)
  
  env %>% add_dataframe("patient", patient_traj, arrivals_df)
  env %>% run(until = simulation_time)
  return(env)
}

set.seed(123)
env_a <- simulate_hospital(num_doctors = 4, num_stretchers = 6,
                           simulation_time = 600, prioritized = FALSE, arrival_rate = 12/60)
arrivals_a <- get_mon_arrivals(env_a, per_resource = TRUE)
resources_a <- get_mon_resources(env_a)

avg_wait_a <- mean(as.numeric(as.character(arrivals_a$waiting_time)), na.rm = TRUE)
max_stretcher_queue <- resources_a %>% 
  filter(resource == "stretcher") %>% 
  summarize(max_q = max(queue)) %>% pull(max_q)

# Aproximación de la inactividad de médicos (utilizamos el máximo de médicos en servicio)
doctor_usage <- resources_a %>% filter(resource == "doctor")
doctor_busy_time <- max(doctor_usage$server)
idle_percentage <- (600 - doctor_busy_time) / 600 * 100

cat("-------------------------------------------------\n")
cat("Escenario (a): Sistema Actual (4 médicos, 6 camillas)\n")
cat("Tiempo promedio de espera en fila (min):", round(avg_wait_a, 2), "\n")
cat("Número máximo en cola para camillas:", max_stretcher_queue, "\n")
cat("Porcentaje aproximado de inactividad de médicos:", round(idle_percentage, 2), "%\n")
cat("-------------------------------------------------\n\n")

set.seed(123)
env_nh <- simulate_hospital_nh(num_doctors = 4, num_stretchers = 6,
                               simulation_time = 600, prioritized = FALSE)
arrivals_nh <- get_mon_arrivals(env_nh, per_resource = TRUE)
resources_nh <- get_mon_resources(env_nh)

avg_wait_nh <- mean(as.numeric(as.character(arrivals_nh$waiting_time)), na.rm = TRUE)
max_stretcher_queue_nh <- resources_nh %>% 
  filter(resource == "stretcher") %>% 
  summarize(max_q = max(queue)) %>% pull(max_q)

cat("-------------------------------------------------\n")
cat("Análisis de Sensibilidad: Llegadas no homogéneas\n")
cat("Entre 8 PM y 12 AM se usan 15 pacientes/hora.\n")
cat("Tiempo promedio de espera en fila (min):", round(avg_wait_nh, 2), "\n")
cat("Número máximo en cola para camillas:", max_stretcher_queue_nh, "\n")
cat("-------------------------------------------------\n\n")

# Gráfico 1: Médicos vs. Tiempo 
p1 <- resources_a %>% 
  filter(resource == "doctor") %>%
  ggplot(aes(x = time, y = server)) +
  
  geom_line(color = "#00E5FF", size = 1.5, linetype = "solid") + 
  
  geom_point(color = "#FFEB3B", size = 3, alpha = 0.8) + 
  
  geom_area(fill = "#00BFA5", alpha = 0.3) +
 
  labs(
    title = "Ocupación de Médicos",
    subtitle = "Visualización optima",
    x = "Tiempo (minutos)",
    y = "Médicos en servicio",
    caption = "Fuente: Simulación del Hospital"
  ) +
  
  theme_dark(base_size = 15) +
  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#00E5FF"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#FFEB3B"),
    axis.title = element_text(face = "bold", color = "#FFFFFF"),
    axis.text = element_text(size = 12, color = "#BDBDBD"),
    panel.grid.major = element_line(color = "#424242", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#121212", color = NA), # Fondo negro elegante
    panel.background = element_rect(fill = "#121212", color = NA)
  )


# Gráfico 2: Cola de Camillas vs. Tiempo 
p2 <- resources_nh %>% 
  filter(resource == "stretcher") %>%
  ggplot(aes(x = time, y = queue)) +
  
  geom_line(color = "#FF5733", size = 1.5, linetype = "solid") +
  geom_point(color = "#FFC300", size = 3, alpha = 0.8) +
  geom_area(fill = "#FF5733", alpha = 0.3) +
  
  labs(
    title = "Evolución de la Cola de Camillas en el Tiempo",
    subtitle = "Visualización optima",
    x = "Tiempo (minutos)",
    y = "Pacientes en Espera",
    caption = "Fuente: Simulación del Hospital"
  ) +
  
  theme_dark(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#FF5733"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "#FFC300"),
    axis.title = element_text(face = "bold", color = "#FFFFFF"),
    axis.text = element_text(size = 12, color = "#BDBDBD"),
    panel.grid.major = element_line(color = "#424242", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#121212", color = NA),  # Fondo negro elegante
    panel.background = element_rect(fill = "#121212", color = NA)
  )

print(p1)
print(p2)
