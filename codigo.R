### Exploracion Base de datos morfologica Aves de paramo
### Referencia: Montoya, P., Gonzalez, M. A., Tenorio, E. A., López‐Ordóñez, J. P., Pinto Gómez, A., Cueva, D., ... & Salgado‐Negret, B. (2018).
### A morphological database for 606 Colombian bird species.

#instalar paquetes sino se tienen usando install.packages("nombre del paquete")
#cargar paquetes   
suppressPackageStartupMessages(library(tidyverse))
library(readxl)
library(raster)
library(sp)
library(leaflet)
library(maps)

# importar base de datos
data <- read_tsv("datos/Montoya_etal_2018_database.txt")

# revisar estructura
str(data)  
dim(data) #9892 obs of 38 variables

# revisar que datos tienen informacion especifica de campo
data %>% 
  group_by(ParamoComplex) %>% 
  summarise(number_records = n(),
            number_sp = n_distinct(Species))

# crear subconjunto de datos para complejo La Cocha
lacocha <- data %>% filter(ParamoComplex == "Complejo de Paramos La Cocha-Patascoy (LCP)")

# revisar informacion para este complejo
lacocha %>% 
  group_by(Locality) %>% 
  summarise(number_records = n(),
            number_sp = n_distinct(Species))

# extraer puntos gps para la cocha
coordinates <- lacocha %>% 
  distinct(decimalLongitude, decimalLatitude, Elevation)

# crear vectores con las coordenadas para visualizacion
ID <- 1:15 # vector del numero de puntos
long <- coordinates$decimalLongitude
lat <- coordinates$decimalLatitude

# visualizar estos puntos de GPS de la cocha
leaflet(data = coordinates) %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~decimalLongitude, ~decimalLatitude, label=as.character(ID))

### Descargar informacion de WorldClim para Colombia con la mayor resolucion posible
# descargar informacion climatica y de elevacion

r <- getData("worldclim", var = "bio", res = 0.5, lon = -77, lat = 1)
elevacion <- getData("worldclim", var="alt", res=0.5, lon=-77, lat=1)

# visualizar perfil de elevacion
plot(elevacion) # plots tile with data downloaded from worldclim

plot(elevacion, xlab="Longitude", ylab="Latitude", 
     ylim=c(min(lat), max(lat)),
     xlim=c(min(long), max(long)))  #plot elevation profile for specific coordinates

# crear puntos espaciales para las coordenadas
points <- SpatialPoints(coordinates, proj4string = r@crs)

# hay 19 variables en wordlclim pero vamos a extraer solo la numero 1 y 12 que son Temp media anual y precipitacion anual
r <- r[[c(1, 12)]]
names(r) <- c("Tmean", "Prec")

# extraer estas dos variables para los puntos
clim <- extract(r, points)

# unir coordenadas y variables climaticas y transformar temperatura media que en worldclim esta multiplicada x10
climate <- cbind.data.frame(coordinates, clim)
climate <- mutate(climate, MAT=Tmean/10) 

# subconjunto final
print(climate)
climate %>% arrange(Elevation) %>% View()
