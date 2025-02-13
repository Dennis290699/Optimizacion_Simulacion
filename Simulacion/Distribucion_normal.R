# Simulación de una distribución normal

# Simulación de 1000 valores de una distribución normal con media 0 y desviación estándar 1
set.seed(123) # Para reproducibilidad
normales <- rnorm(1000, mean = 0, sd = 1)
hist(normales, probability = TRUE)


# Simulación de un experimento de Bernoulli

# Simulación de 1000 experimentos de Bernoulli con probabilidad de éxito 0.4
set.seed(123) # Para reproducibilidad
bernoulli <- rbinom(1000, size = 1, prob = 0.4) #size un experimento 1 ensayo 
table(bernoulli)
bernoulli <- rbinom(1000, size = 2, prob = 0.4) #size un experimento 1 ensayo 
table(bernoulli)#0 exitos 1exite y 2exitos
barplot(table(bernoulli), 
        main = "Distribución de una Binomial (n=2, p=0.4)", 
        xlab = "Número de éxitos", 
        ylab = "Frecuencia", 
        col = "skyblue")


# Simulación de una distribución de Poisson

# Simulación de 1000 valores de una distribución de Poisson con parámetro lambda 2
set.seed(123) # Para reproducibilidad
poisson <- rpois(1000, lambda = 2)
poisson <- rpois(1000, lambda = 1/2)

hist(poisson, probability = TRUE)


# Simulación de un modelo lineal

# Simulación de 1000 valores de un modelo lineal con coeficientes beta0 = 2 y beta1 = 3
set.seed(123) # Para reproducibilidad
x <- runif(1000, min = 0, max = 10)
y <- 2 + 3 * x + rnorm(1000, mean = 0, sd = 1)

plot(x, y)

#distrucion exponencial 

set.seed(123)  # Para reproducibilidad
lambda <- 0.5  # Tasa de la distribución exponencial (lambda)
n <- 100       # Cantidad de números a generar
# Generación de los números con distribución exponencial
numeros_exponenciales <- rexp(n, rate = lambda)

# Mostrar los primeros 10 números generados
print(head(numeros_exponenciales, 10))

# Graficar un histograma de los números generados
hist(numeros_exponenciales, breaks = 20, probability = TRUE, col = "lightblue",
     main = "Distribución Exponencial (lambda = 0.5)",
     xlab = "Valores", ylab = "Densidad")

# Agregar la curva teórica de la distribución exponencial
curve(dexp(x, rate = lambda), add = TRUE, col = "red", lwd = 2)
