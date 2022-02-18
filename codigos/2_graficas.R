### 23 Noviembre 2021
### graficas exploratorias para el subset de coordenadas y variables ambientales

#cargar paquetes   
suppressPackageStartupMessages(library(tidyverse))

#importar subset de datos
subset <- read_csv("sub_datos/coordinates.csv")

# temperatura declina linealmente con elevacion
ggplot(subset, aes(Elevation, Tmean)) +
  geom_point() +
  theme_bw()

# pero precipitacion no tiene un patron claro
ggplot(subset, aes(Elevation, Prec)) +
  geom_point() +
  theme_bw()

# Algunas localidades se asemejan en condiciones ambientales 
# a pesar de estar en elevaciones diferentes
ggplot(subset, aes(Tmean, Prec, color = Elevation)) +
  scale_color_gradient(low="green", high="red") +
  geom_point(size = 2) +
  theme_bw()

# o en complejos de paramo diferentes
ggplot(subset, aes(Tmean, Prec)) +
  geom_point(aes(color = factor(ParamoComplex)), size = 2) +
  theme_bw()

# o departmentos
ggplot(subset, aes(Tmean, Prec)) +
  geom_point(aes(color = factor(Department)), size = 2) +
  theme_bw()




