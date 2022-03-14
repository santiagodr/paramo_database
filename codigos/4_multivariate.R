### 07-14 Marzo 2022

# Codigo para realizar PCA (Principal component analysis) y HC (Hierarchical clustering)
# en los datos de worldclim de las coordenadas unicas en la base de datos
# el objetivo es identificar "clusters" de localidades similares ambientalmente

# notes Climatic variables have different meanings and different ranges of values. 
# To contribute equally to the value of dissimilarity between two LCs they need to be 
# scaled to have identical ranges [using normalization] or similar ranges

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
