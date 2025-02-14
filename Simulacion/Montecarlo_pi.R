# Estimación de π usando el método de Montecarlo

set.seed(123)  # Fijar semilla para reproducibilidad

# Número de puntos a generar
n_puntos <- 10000  

# Generar puntos aleatorios en el cuadrado [-1,1] x [-1,1]
x <- runif(n_puntos, min = -1, max = 1)
y <- runif(n_puntos, min = -1, max = 1)

# Contar cuántos puntos caen dentro del círculo de radio 1
dentro_circulo <- x^2 + y^2 <= 1
n_dentro <- sum(dentro_circulo)

# Estimación de π
pi_estimado <- (n_dentro / n_puntos) * 4

# Mostrar resultado
print(paste("Estimación de π con", n_puntos, "puntos:", round(pi_estimado, 5)))

# Graficar los puntos
library(ggplot2)

datos <- data.frame(x, y, dentro_circulo)

ggplot(datos, aes(x = x, y = y, color = dentro_circulo)) +
  geom_point(size = 0.5) +
  labs(title = paste("Estimación de π: ", round(pi_estimado, 5)),
       subtitle = paste("Puntos dentro del círculo:", n_dentro, "/", n_puntos),
       x = "X", y = "Y") +
  scale_color_manual(values = c("red", "blue"), labels = c("Fuera", "Dentro")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  