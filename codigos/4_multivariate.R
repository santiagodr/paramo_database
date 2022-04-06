### 07-14 Marzo 2022

# Codigo para realizar PCA (Principal component analysis) y HC (Hierarchical clustering)
# en los datos de worldclim de las coordenadas unicas en la base de datos
# el objetivo es identificar "clusters" de localidades similares ambientalmente

# instalar paquetes sino se tienen usando 
# install.packages(c("FactoMineR", "factoextra"))

# cargar paquetes
suppressPackageStartupMessages(library(tidyverse))
library(FactoMineR)
library(factoextra)

#cargar datos
envdata <- read_csv("sub_datos/coordinates_wc_19variables.csv")

# explorar variacion y rango de las variables que estan en las columnas 7 a 25
x=envdata[,7:25] 
boxplot(x)  #bio12_23 tiene un rango y varianza muy amplia comparada al resto

# PARTE 1. construir PCA, manteniendo los 3 primeros componentes y escalando las variables
# debido a las diferencias entre bio12_23 y el resto.

res.pca <- PCA(envdata[,7:25], scale.unit = TRUE, ncp = 3, graph = F) 

# el resultado de esto es una lista de objetos con toda la informacion 
print(res.pca)

# podemos explorar por ejemplo...
plot(res.pca) #como se distribuyen las localidades
plot(res.pca, choix = "var") # como se relacionan las variables

# Los primeros dos componentes resumen el 77% de la variabilidad en los datos
plot(res.pca, choix = "var") 
ggsave("figuras/pca19variables.pdf") #guardamos esta ultima

# revisar dimensiones y variables con peso en cada componente
dimdesc(res.pca, axes = c(1,2))

#1.1 Explorar otro PCA incluyendo "elevacion" como variable cuantitativa suplementaria, que esta en la posicion 3 
res.pca2 <- PCA(envdata[,c(3,7:25)], scale.unit=TRUE, ncp=5, quanti.sup= 1, graph=T)

#1.2 Explorar otro PCA incluyendo "complejos" como variable qualitativa suplementaria, que esta en la posicion 4
res.pca3 <- PCA(envdata[,c(4,7:25)], scale.unit=TRUE, ncp=5, quali.sup= 1, graph=F)

plot.PCA(res.pca3, axes=c(1, 2), choix="ind", habillage=1)
plotellipses(res.pca3)
ggsave("figuras/pca_complejos.pdf", width = 8, height = 6) #guardamos esta ultima


#### PARTE 2. Usar el primer PCA y realizar HC sobre los componentes principales

# la funcion HCPC de FactoMineR sugiere un punto de corte para seleccionar el numero de 
# clusters y pide que se de click en ese punto
res.hcpc <- HCPC(res.pca, graph = T) 

# En este caso sugiere 5 clusters de localidades que resumen las diferencias en
# variables ambientales. Tambien se puede usar "graph = F" y dejar que la funcion
# selecciona sin cortar manualmente

# Otras funciones del paquete "factoextra" para visualizar los mismos resultados
# Dendrograma de clusters
fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
          )
ggsave("figuras/hc_clusters.pdf")

# Clusters de localidades minimizando sobrelapamiento
fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)
ggsave("figuras/hc_localidades.pdf")

# los "clusters" identificados para cada localidad estan en el objeto res.hcpc$data.clust$clust
res.hcpc$data.clust$clust

# ahora incorporamos esos clusters al subset de datos inicial
envdata_clusters <- envdata %>% 
  mutate(clusters = res.hcpc$data.clust$clust)

# y podemos visualizar otras variables con esta asignación de clusters
ggplot(envdata_clusters, aes(bio1_23, bio12_23, color = clusters)) +
  geom_point(size = 2) +
  theme_bw()

# por ejemplo aca podemos ver que al graficar temperatura media anual "bio1" 
# y Precipitacion anual "bio12", el cluster 2 son localidades frias y con poca precipitacion, 
# mientras que el cluster 3 son localidades mas calidas y lluviosas. 

# NOTA: se deben revisar diferentes resultados tanto del PCA como del HC para entender
# que variables dominan los componentes y la asignacion de clusters, el cluster 5 por ejemplo, 
# no es tan definido y se sobrelapa con el 4 y el 3.


# 31_Marzo_2022
# continuacion de exploracion de resultados del analisis de cluster 

# clusters por complejo de paramo
envdata_clusters %>%
  distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>% 
  group_by(ParamoComplex, clusters) %>%
  summarise(n()) %>% 
  pivot_wider(names_from = clusters, values_from = `n()`)

#la mayoria de complejos tienen localidades en dos clusters, pero dos complejos
# tienen puntos hasta en 4 clusters: la cocha patascoy y chili barragan

# Complejo de Paramos Chili-Barragan (CBG)
library(leaflet)
envdata_clusters %>%
  filter(ParamoComplex == "Complejo de Paramos Chili-Barragan (CBG)") %>% 
  distinct(decimalLongitude, decimalLatitude, Elevation) %>% 
  leaflet() %>% 
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  addCircleMarkers(~decimalLongitude, ~decimalLatitude)

# Este complejo tiene puntos muy alejados en diferentes departamentos, por lo tanto 
# es posible que la alta variación climatica se deba a la separacion geografica


#### PARTE 3 ####
# extraer informacion del PCA usando funciones de "FactoMineR"
# para mas informacion explorar el link
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

# extraer diferentes parametros
eigen <- get_eigenvalue(res.pca)
var <- get_pca_var(res.pca)
var$coord
var$cos2
var$contrib

# visualizar diferentes parametros
fviz_pca_var(res.pca, col.var = "black")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

#contribucion de las variables en los primeros 2-3 componentes
corrplot(var$contrib, is.corr=FALSE)  

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 15)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

# Este codigo se puede usar si se desean cambiar los nombres de las variables de worldclim
envdata %>% 
  rename(Annual_Mean_Temp = bio1_23,
         Mean_Diurnal_Range = bio2_23,
         Isothermality = bio3_23,
         Temp_seasonality = bio4_23,
         Max_Temp_warmest_month = bio5_23,
         Min_Temp_coldest_month = bio6_23,
         Temp_annual_range = bio7_23,
         Mean_Temp_wettest_quarter = bio8_23,
         Mean_Temp_driest_quarter = bio9_23,
         Mean_Temp_warmest_quarter = bio10_23,
         Mean_Temp_coldest_quarter = bio11_23,
         Annual_Precipitation = bio12_23,
         Prec_wettest_month = bio13_23,
         Prec_driest_month = bio14_23,
         Prec_seasonality = bio15_23,
         Prec_wettest_quarter = bio16_23,
         Prec_drier_quarter = bio17_23,
         Prec_warmest_quarter = bio18_23,
         Prec_coldest_quarter = bio19_23)

#### PARTE 4 ####
# Exportar subset de datos solo de informacion de localidad y clusters, reordenar
# y dar nombre unico a cada localidad (es decir combinacion de longitud y latitud unicas)
# este nombre tendra las siglas del complejo de paramo y un numero unico.

locality <- 1:483 #crear vector

coordinates_clusters <- envdata_clusters %>% 
  arrange(ParamoComplex, Department, Locality) %>%
  mutate(complex_code = str_extract(ParamoComplex, "\\(.+\\)"), #extraer codigo
         number_locality = locality, #adicionar numero unico
         unique_locality = paste(complex_code, number_locality, sep = "_") #unirlos
         ) %>% 
  select("decimalLongitude", "decimalLatitude", "Elevation", "ParamoComplex",
       "Department", "Locality", "clusters", "unique_locality")

write_csv(coordinates_clusters, "sub_datos/coordinates_clusters.csv")