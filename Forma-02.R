#Forma-02

#Librerías
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

#Lectura de los datos de entrada
datos <- read.csv2(file.choose(), head = TRUE, encoding = "UTF-8")

#PREGUNTA 1

#Se seleccionan las casas y los puntajes del primer trimestre
datost1 <- select(datos, casa, trim1)

#Filtro de datos para trabajar con las casas Hufflepuff y Ravenclaw
casaH <- filter(datost1, casa =='Hufflepuff')
casaR <- filter(datost1, casa =='Ravenclaw')

#Semilla
set.seed(531)

#Nivel de significación
alfa <- 0.05

#Hipotesis

#H0: Hufflepuff (h) y Ravenclaw (r) tienen una diferencia promedio de 22 
#puntos para el primer trimestre (Xh - Xr == 22)

#Ha: Hufflepuff (h) y Ravenclaw (r) tienen una diferencia promedio distinta de
#22  puntos para el primer trimestre (Xh - Xr =/= 22)


#Se juntan los datos de ambas casas en un data frame
n_H <- length(casaH$trim1)
n_R <- length(casaR$trim1)
casa <- c(rep("Hufflepuff", n_H), rep("Ravenclaw", n_R))
puntos <- c(casaH$trim1,casaR$trim1)
datoshr <- data.frame(casa,puntos)

#PREGUNTA 2

#PREGUNTA 3