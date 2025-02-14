# Simulación de 1000 lanzamientos de un dado
set.seed(123)
dados <- sample(1:6, 1000, replace = TRUE)

# Cálculo de frecuencias
frecuencia_absoluta <- table(dados)
frecuencia_relativa <- prop.table(frecuencia_absoluta)
frecuencia_acumulada <- cumsum(frecuencia_relativa)

# Mostrar resultados
print("Frecuencia absoluta:")
print(frecuencia_absoluta)

print("Frecuencia relativa:")
print(frecuencia_relativa)

print("Frecuencia acumulada:")
print(frecuencia_acumulada)

# Visualización
barplot(frecuencia_absoluta, main = "Frecuencia Absoluta de un Dado",
        xlab = "Cara del Dado", ylab = "Frecuencia", col = "blue")

barplot(frecuencia_relativa, main = "Frecuencia Relativa de un Dado",
        xlab = "Cara del Dado", ylab = "Frecuencia Relativa", col = "green")

pie(frecuencia_relativa, 
    labels = paste(names(frecuencia_relativa), "(", round(100 * frecuencia_relativa, 2), "%)", sep = ""),
    main = "Frecuencia Relativa (Gráfico de Pastel)", 
    col = rainbow(length(frecuencia_relativa)))

plot(names(frecuencia_acumulada), frecuencia_acumulada, type = "o", col = "red",
     main = "Frecuencia Acumulada de un Dado",
     xlab = "Cara del Dado", ylab = "Frecuencia Acumulada", lwd = 2, pch = 16)

barplot(frecuencia_acumulada, 
        main = "Frecuencia Acumulada de un Dado",
        xlab = "Cara del Dado", 
        ylab = "Frecuencia Acumulada", 
        col = "purple")

pie(frecuencia_acumulada, 
    labels = paste(names(frecuencia_acumulada), "(", round(100 * frecuencia_acumulada, 2), "%)", sep = ""),
    main = "Frecuencia Acumulada (Gráfico de Pastel)", 
    col = rainbow(length(frecuencia_acumulada)))
    