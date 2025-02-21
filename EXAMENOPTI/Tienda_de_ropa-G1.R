set.seed(123)  # Para reproducibilidad

# Definir parámetros
n_clientes <- 200
hora_apertura <- 10  # 10 AM
hora_cierre <- 20    # 8 PM
duracion_tienda <- hora_cierre - hora_apertura

# Generar tiempos de llegada
llegadas <- abs(rnorm(n_clientes, mean = 0, sd = 1))
llegadas <- sort(hora_apertura + (llegadas / max(llegadas)) * duracion_tienda)

# Asignar tipos de clientes y tiempos de compra
tipo_cliente <- sample(c("Mirar", "Pequeño", "Grande"), n_clientes, replace = TRUE, prob = c(0.3, 0.5, 0.2))
tiempo_compra <- numeric(n_clientes)
tiempo_compra[tipo_cliente == "Mirar"] <- 0
tiempo_compra[tipo_cliente == "Pequeño"] <- rexp(sum(tipo_cliente == "Pequeño"), rate = 1/10)
tiempo_compra[tipo_cliente == "Grande"] <- runif(sum(tipo_cliente == "Grande"), min = 15, max = 30)

# Calcular la hora de salida
salidas <- llegadas + tiempo_compra / 60

# Calcular concurrencia por minuto
tiempo_minutos <- seq(hora_apertura, hora_cierre, by = 1/60)
concurrencia <- sapply(tiempo_minutos, function(t) sum(llegadas <= t & salidas > t))

# Encontrar la hora más concurrida
hora_pico <- tiempo_minutos[which.max(concurrencia)]
cat("Hora más concurrida:", floor(hora_pico), ":", round((hora_pico - floor(hora_pico)) * 60), "\n")

# Gráficos
library(ggplot2)

data_concurrencia <- data.frame(Hora = tiempo_minutos, Concurrencia = concurrencia)
ggplot(data_concurrencia, aes(x = Hora, y = Concurrencia)) +
  geom_line(color = "blue") +
  labs(title = "Concurrencia en la tienda", x = "Hora del día", y = "Número de clientes") +
  theme_minimal()

data_clientes <- data.frame(Cliente = 1:n_clientes, TiempoTotal = (salidas - llegadas) * 60)
ggplot(data_clientes, aes(x = Cliente, y = TiempoTotal)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Tiempo total en la tienda por cliente", x = "Cliente", y = "Tiempo (min)") +
  theme_minimal()

# Tabla de datos
library(dplyr)
library(knitr)
data_tabla <- data.frame(Cliente = 1:n_clientes, Hora_Llegada = llegadas, Tipo = tipo_cliente, Tiempo_Compra = tiempo_compra, Hora_Salida = salidas)
kable(head(data_tabla, 20), caption = "Primeros 20 clientes en la tienda")
