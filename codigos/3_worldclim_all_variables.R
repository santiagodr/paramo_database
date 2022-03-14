## 03-Mar-22
## extraer todas las 19 variables de Worldclim para las coordenadas unicas en la base de datos

# cargar paquetes
suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(raster)

# cargar datos
field <- read_csv("sub_datos/paramo_field.csv")

# extraer puntos unicos de gps para este subset de datos y algunas variables de localidad
coordinates <- field %>% 
  distinct(decimalLongitude, decimalLatitude, Elevation, ParamoComplex, Department, Locality)

# definir datos de worldclim para la cuadricula de Colombia
r <- getData("worldclim", var = "bio", res = 0.5, lon = -77, lat = 1)

# crear informacion espacial para las 483 coordenadas combinadas
points <- SpatialPoints(coordinates[,c("decimalLongitude","decimalLatitude")], proj4string = r@crs)

# extraer todas las 19 variables de WC, y unirlas a las coordenadas
clim <- extract(r, points)
climate <- cbind.data.frame(coordinates, clim)

# exportar
write_csv(climate, "sub_datos/coordinates_wc_19variables.csv")




