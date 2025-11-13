# SilviaGarcia_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento
# Con la función getwd() miro si he cambiado correctamente el directorio a la carpeta en la que tengo el trabajo
getwd()
# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
#Tengo que cargar los paquetes que voy a necesitar para instalarlos
paquetes <- c("readr", "ggplot2")
# Las instalo
instalar <- paquetes[!paquetes %in% installed.packages()]
if(length(instalar)) install.packages(instalar)

# Activo las librerías para poder usarlas 
lapply(paquetes, library, character.only = TRUE)

# Importo el archivo en donde se encuentran los datos que voy a analizar
datos <- read_csv("datos_biomed.csv")

# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
# Directamente con escribir las funciones: 
head(datos)
# head(): me va a dejar visualizar las primeras filas del contenido
 
summary(datos)
# con summary(): me muestra un resumen general

dim(datos)
# dim(): me muestra el número de filas y columnas

str(datos)
#str(): tipo de variables 

#Para saber cuántas variables hay, es decir, cuántas columnas hay
ncol(datos)
#Para saber cuántos tratamientos hay 
length(unique(datos$Tratamiento))
#Para ver exactamente que tres tratamientos hay
unique(datos$Tratamiento)

# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)
# Para que los resultados, es decir, las 3 gráficas me salgan en la misma imagen utilizo la función par(mfrow=c(1,3))
par(mfrow=c(1,3))
#Hago el boxplot de glucosa con los tratamientos
boxplot(Glucosa ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"), #col para poner un color a cada tratamiento 
        main = "Distribución de Glucosa por tratamiento", #título del gráfico
        xlab = "Tratamientos", ylab = "Glucosa (mg/dL)")# Las variables del eje x y eje y

# Lo mismo con los datos de Presión y Colesterol
boxplot(Presion ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Distribución de Presión por tratamiento",
        xlab = "Tratamientos", ylab = "Presión (mmHg)")

boxplot(Colesterol ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Distribución de Colesterol por tratamiento",
        xlab = "Tratamientos", ylab = "Colesterol (mg/dL)")


# 4. Realiza un violin plot (investiga qué es). (1 pt)
# Como antes no había instalado este paquete de vioplot lo instalo ahora y activo la librería para poder utilizarlo
install.packages("vioplot")
library(vioplot)
# De nuevo usamos la función par(mfrow =c (1,3)) para que me salgas los 3 violin plot en la misma imagen
par(mfrow = c(1, 3))

# Para la Glucosa de nuevo usando col, main y xlab e ylab
vioplot(Glucosa ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Distribución de Glucosa por tratamiento",
        xlab = "Tratamientos", ylab = "Glucosa (mg/dL)")

# Presión
vioplot(Presion ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Distribución de Presión por tratamiento",
        xlab = "Tratamientos", ylab = "Presión (mmHg)")

# Colesterol
vioplot(Colesterol ~ Tratamiento, data = datos,
        col = c("lightblue", "lightgreen", "lightpink"),
        main = "Distribución de Colesterol por tratamiento",
        xlab = "Tratamientos", ylab = "Colesterol (mg/dL)")

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
# Hago un gráfico de dispersión para ver la relación entre las dos variables 
plot(datos$Glucosa, datos$Presion,
     col = as.numeric(factor(datos$Tratamiento)),  
     pch = 19,                                      
     main = "Glucosa vs Presión",
     xlab = "Glucosa (mg/dL)",
     ylab = "Presión (mmHg)")

legend("bottomright",
       legend = levels(factor(datos$Tratamiento)),  
       col = 1:length(unique(datos$Tratamiento)),  
       pch = 19, 
       title = "Tratamientos")

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
# Primero tengo que cargar la librería ggplot2 que ya había instalado al principio 

library(ggplot2)
ggplot(datos, aes(x = Presion, y = Colesterol, color = Tratamiento)) +
  geom_point(size = 2) + # Para los puntos de dispersión                    
  geom_smooth(method = "lm", se = FALSE) +   
  facet_wrap(~ Tratamiento) +                
  labs(title = "Relación entre Colesterol y Presión por tratamiento",
       x = "Presión (mmHg)",
       y = "Colesterol (mg/dL)") +
  theme_minimal()
#Un facet Grid es una técnica de visualización que permite dividir un gráfico en varios subgráficos por cada categoría de una variable

# 7. Realiza un histogramas para cada variable. (0.5 pts)
# Otra vez igual que el boxplot por ejemplo pero con la función hist() para realizar los histogramas 
par(mfrow = c(1, 3))
hist(datos$Glucosa,
     main = "Histograma de Glucosa",
     col = "lightblue",
     xlab = "Glucosa (mg/dL)")
hist(datos$Presion,
     main = "Histograma de Presión",
     col = "lightgreen",
     xlab = "Presión (mmHg)")
hist(datos$Colesterol,
     main = "Histograma de Colesterol",
     col = "lightpink",
     xlab = "Colesterol (mg/dL)")

# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)
# Tranforma una columna de texto en una variable categórica
#Convierto la variable Tratamiento en un factor 
factor(datos$Tratamiento)
#Para ver el orden y las categorías con levels()
levels(factor(datos$Tratamiento))

# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)
# Utilizo la función aggregate para calcular tanto la media con mean como la desviación estándar con sd
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean)
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)
# Para extraer los datos utilizo subset y los añado a una variable independiente
placebo  <- subset(datos, Tratamiento == "Placebo")
farmacoA <- subset(datos, Tratamiento == "FarmacoA")
farmacoB <- subset(datos, Tratamiento == "FarmacoB")
#Para ver un ejemplo del contenido de uno de ellos 
head(placebo)

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)
# Para comprobar si los datos siguen una distribución normal o no utilizo shapiro test para cada uno (glucosa, colesterol y presión) vs los 3 tratamientos diferentes
# Si p > 0,05 será distribución normal 
shapiro.test(placebo$Glucosa)
shapiro.test(farmacoA$Glucosa)
shapiro.test(farmacoB$Glucosa)

#Los datos siguen una distribución normal porque p >0,05, asi que utilizamos t-test

t.test(placebo$Glucosa,farmacoA$Glucosa)
t.test(placebo$Glucosa,farmacoB$Glucosa)
t.test(farmacoB$Glucosa,farmacoA$Glucosa)

shapiro.test(placebo$Presión)
shapiro.test(farmacoA$Presión)
shapiro.test(farmacoB$Presión)

t.test(placebo$Glucosa,farmacoA$Presión)
t.test(placebo$Glucosa,farmacoB$Presión)
t.test(farmacoB$Glucosa,farmacoA$Presión)

shapiro.test(placebo$Colesterol)
shapiro.test(farmacoA$Colesterol)
shapiro.test(farmacoB$Colesterol)

t.test(placebo$Glucosa,farmacoA$Colesterol)
t.test(placebo$Glucosa,farmacoB$Colesterol)
t.test(farmacoB$Glucosa,farmacoA$Colesterol)

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)
# ANOVA para glucosa y cada tratamiento 
anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)
# Resumen del análisis 
summary(anova_glucosa)
# Usamos TukeyHSD para hacer una comparación múltiple de medias con el análisis del ANOVA para ver las diferencias significativas entre los diferentes tratamientos
TukeyHSD(anova_glucosa)

