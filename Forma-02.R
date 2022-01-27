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
