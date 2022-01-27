#Forma-02

#Librerías:
if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}
if (!require(tidyverse) ) {
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(ez) ) {
  install.packages("ez", dependencies = TRUE )
  require (ez)
}
if (!require(boot) ) {
  install.packages("boot", dependencies = TRUE )
  require (boot)
}
if (!require(simpleboot) ) {
  install.packages("simpleboot", dependencies = TRUE )
  require (simpleboot)
}
if (!require(bootES) ) {
  install.packages("bootES", dependencies = TRUE )
  require (bootES)
}

#Lectura de los datos de entrada:
datos <- read.csv2(file.choose(), head = TRUE, encoding = "UTF-8")

#PREGUNTA 1

#Se seleccionan las casas y los puntajes del primer trimestre:
datost1 <- select(datos, casa, trim1)

#Filtro de datos para trabajar con las casas Hufflepuff y Ravenclaw:
casaH <- filter(datost1, casa =='Hufflepuff')
casaR <- filter(datost1, casa =='Ravenclaw')

#Semilla:
set.seed(531)

#Nivel de significación:
alfa <- 0.05

#Hipotesis:

#Se pide estudiar la media de los puntajes de estudiantes 2 casas

#H0: Hufflepuff (h) y Ravenclaw (r) tienen una diferencia promedio de 22 
#puntos para el primer trimestre (Xh - Xr == 22)

#Ha: Hufflepuff (h) y Ravenclaw (r) tienen una diferencia promedio distinta de
#22  puntos para el primer trimestre (Xh - Xr =/= 22)


#Se juntan los datos de ambas casas en un data frame:
n_H <- length(casaH$trim1)
n_R <- length(casaR$trim1)
casa <- c(rep("Hufflepuff", n_H), rep("Ravenclaw", n_R))
puntos <- c(casaH$trim1,casaR$trim1)
datoshr <- data.frame(casa,puntos)

#Comprobación de normalidad (Shapiro test):
print(shapiro.test(casaH$trim1))
print(shapiro.test(casaR$trim1))

#El valor p entregado por el shapiro test para cada casa es alejado del valor de
#alfa definido

#Diferencia de medias entre ambas casas:
mediaH <- mean(casaH$trim1)
mediaR <- mean(casaR$trim1)
diffHR <- mediaH - mediaR

cat ("diferencia observada:", mediaH - mediaR,"\n\n")

#Distribución bootstrap:
#5000 repeticiones
B <- 5000

#Valor nulo:
valor_nulo <- 22

distribucion_bootstrap <- two.boot(casaH$trim1,casaR$trim1,FUN = mean, R = B)

#Se analiza la distribución bootstrap:
valores <- data.frame(distribucion_bootstrap$t)
colnames(valores) <- "valores"

#Histograma de frecuencias:
histograma <- gghistogram(valores, x = "valores", color = "red",
                             fill = "red", bins = 100 ,
                             xlab = "Diferencia de medias",
                             ylab = "Frecuencia", add = "mean")
print(histograma)

#Grafico QQ:
qq <- ggqqplot(valores, x = "valores", color = "red")
print(qq)

cat("Distribución bootstrap:\n")
cat ("\tMedia:", mean(valores$valores),"\n")
cat ("\tDesviación estándar:",sd(valores$valores),"\n\n")

#En el análisis de los gráficos se puede ver que hay una distribución de los 
#datos cercana a la normal

#Cálculo del p valor:

desplazamiento <- mean(distribucion_bootstrap[["t"]]) - valor_nulo
distribucion_nula <- distribucion_bootstrap [["t"]] - desplazamiento

#Se determina el p valor
p <- (sum(abs(distribucion_nula) > abs(diffHR)) + 1) / (B + 1)
cat ("Valor p:", p)

#Como el p valor es 0.2405519, mayor que el nivel de significacion
#alfa = 0.05, se falla en rechazar la hipótesis nula. En consecuencia, se
#concluye con un 95% de confianza que Hufflepuff y Ravenclaw tienen una 
#diferencia promedio de 22  puntos para el primer trimestre

#PREGUNTA 2

#Funciones a utilizar:

# Función para calcular la diferencia de medias .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.

calcular_diferencia <- function (muestra_1, muestra_2, FUN) {
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# FunciÓn para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# Valor :
# - diferencia E_1 - E_2.

permutar <- function (muestra_1, muestra_2, FUN) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = FALSE)
  permutacion_1 <- permutacion [1 : n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]
  return(calcular_diferencia(permutacion_1 , permutacion_2 , FUN))
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales.
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# Valor :
# - el valor p calculado .

calcular_valor_p <- function (distribucion, valor_observado, repeticiones, alternative) {
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  
  else{
    numerador <- sum(distribucion < valor_observado ) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  
  return (valor_p)
}



#PREGUNTA 3