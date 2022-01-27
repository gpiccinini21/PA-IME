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

# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .

graficar_distribucion <- function (distribucion) {
  observaciones <- data.frame(distribucion)
  histograma <- gghistogram (observaciones, x = "distribucion",
                             xlab = "Estadístico de interés",
                             ylab = " Frecuencia ")
  qq <- ggqqplot (observaciones , x = " distribucion ")
  figura <- ggarrange (histograma, qq , ncol = 2 , nrow = 1)
  print (figura)
}

# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : función del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# - plot : si es TRUE , construye el grÃ¡fico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion .
contrastar_hipotesis_permutaciones <- function (muestra_1, muestra_2, repeticiones, FUN, alternative, plot){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa: ", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado: ", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  for (i in 1: repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  
  cat ("Valor p: ", valor_p, "\n\n")
}

#Semilla:
set.seed(847)

#Repeticiones:
n_rep <- 2000

#Nivel de significación:
alfa <- 0.05

#Hipotesis:

#H0: El promedio de los puntos ganados o perdidos por los estudiantes de la casa
#Slytherin durante el primer trimestre es igual al promedio de los puntos
#ganados o perdidos durante el segundo trimestre. (uTrim1 = uTrim2 = 0)

#Ha: El promedio de los puntos ganados o perdidos por los estudiantes de la casa
#Slytherin durante el primer trimestre, es mayor al promedio de los puntos
#ganados o perdidos durante el segundo trimestre. (uTrim1 > uTrim2)

#Se filtran los datos para la casa Slytherin:
Slytherin <- filter(datos, casa == "Slytherin")
trim1 <- Slytherin$trim1
trim2 <- Slytherin$trim2

#Se analiza si la distribucion de los datos cumple con normalidad:

#Histograma trimestre 1
hist(x = trim1, main = "Histograma de Trimestre 1", 
     xlab = "Puntos ganados o perdidos por el estudiante durante el primer trimestre")

#Histograma trimestre 2
hist(x = trim2, main = "Histograma de Trimestre 2",
     xlab = "Puntos ganados o perdidos por el estudiante durante el primer trimestre")

#Analizando ambos gráficos, en el primer trimestre se observa que no se cumple
#normalidad alguna con respecto a la distribución de los datos, por otro lado
#en el segundo trimestre, se observa un acercamiento a la distribución normal
#pero no es clara, y por tanto no tiene una distribución normal.

#Se contrastan las hipótesis con la función creada:
contrastar_hipotesis_permutaciones(trim1,trim2,repeticiones = n_rep, FUN = mean, alternative= "greater", plot = FALSE)

#Tal como se observa en los resultados obtenidos, con un p < alfa = 0.05 se
#puede concluir con un 95% de confianza que el promedio de los puntos ganados
#o perdidos por los estudiantes de la casa Slytherin durante el primer
#trimestre, es mayor al promedio de los puntos ganados o perdidos durante el
#segundo trimestre.

#PREGUNTA 3

#Ejemplo regresión lineal múltiple:

#Se desea predecir las expectativas de los chilenos y chilenas con respecto al
#nuevo gobierno, es por tanto que mediante una investigación, se recolectaron
#datos de 200 personas habitantes de la región Metropolitana, dentro de los
#cuales se encuentran: nombre, edad, nivel socioeconómico, comuna de residencia,
#sector político, expectativa futuro gobierno.

#Variables:
#-nombre: Categórico
#-edad: Numérica
#-nivel socioeconómico: Categórico (por ejemplo: clase baja, clase media, clase alta)
#-comuna de residencia: Categórico (por ejemplo: quinta normal, las condes, renca)
#-sector político: Categórico (por ejemplo: izquierda, centro, derecha)
#-expectativa futuro gobierno: Categórico (por ejemplo: baja, media, alta)

#Hipótesis:
#H0:
#Ha: