### 25-Mayo-2022
# Santiago David

# Codigo para realizar NMDS de los datos de composicion/abundancia de especies por localidad
# El objetivo es identificar si diferentes areas de paramo presentan similaridad en su composicion de especies,
# y si esta similaridad corresponde con los clusters identificados con variables ambientales.

# Hay varias opciones para definir las areas de paramo para este analisis. 
# Los datos de abundancia se pueden calcular por clusters (5), complejos de paramo (18), 
# localidades (85) o localidaes unicas segun el GPS (483), 
# o una combinacion de estos que refleje significativamente la composicion de las comunidades y las unidades de comparacion

# cargar paquetes
library(tidyverse)
library(vegan)

# cargar datos de abundancias por localidad

# NDMS 1 - el primer analisis se hara a nivel de localidades unicas, o en otras palabras, 
# las 483 localidades definidas como coordenadas unicas.

abundance <- read_csv("sub_datos/matriz_abundancias.csv") #Matriz de abundancias
abundance # 179 especies en 483 localidades

# transformar a matriz para usar la funcion metaMDS
data_m <- as.matrix(abundance[, 2:180])

# NMDS1 
set.seed(100)
nmds1 <- metaMDS(data_m, k=2, distance="bray", trymax=20)

nmds1 
plot(nmds1)
stressplot(nmds1)

# resultados: se repitio con 20 y 100 replicas. 
# El mensaje de error indica que puede no haber suficientes datos.
# Los plots y resultados sugieren que puede haber muchas localidades o especies "no informativas", por ejemplo la disimilaridad observada es 1 en muchas ocasiones,
# Es decir:
# en la matriz de abundancias existen muchas especies que estan presentes una sola vez y en una sola localidad, lo cual incrementa la disimilaridad por ser localidades poco informativas,
# Esto es un problema de la cantidad de zeros en la matriz de abundancias, lo cual ya se habia explorado en el script 5.

data.scores <- as.data.frame(scores(nmds1)$sites)

data.scores %>% 
  ggplot(aes(NMDS1, NMDS2)) +
  geom_point(size = 2) +
  theme_classic()
ggsave("figuras/nmds1_matriz de 485 comunidades.pdf")

# NMDS 2 - El segundo analisis se hara a nivel de localidades como estan definidas en la base de datos.
# en lugar de usar las 483 localidades definidas por coordenadas unicas, se creara una matriz de abundancia para las
# localidades unicas definidas en la base de datos (85).

# crear matriz de abundancias

field_data <- read_csv("sub_datos/paramo_field_and_clusters.csv")

field_data %>% distinct(Locality) #85 localidades (i.e comunidades)

abundance2 <- field_data %>% 
  group_by(Species, Locality) %>% 
  summarize(abundance_sp = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = abundance_sp, values_fill = list(abundance_sp = 0)) %>%
  arrange(Locality)

# transformar a matriz para usar la funcion metaMDS
data_m2 <- as.matrix(abundance2[, 2:180])

# NMDS2 
set.seed(100)
nmds2 <- metaMDS(data_m2, k=2, distance="bray", trymax=100)

nmds2 
plot(nmds2)
stressplot(nmds2)

# extraer NMDS scores (x and y coordinates) para visualizar los resultados
data.scores2 <- as.data.frame(scores(nmds2)$sites)

data.scores2 %>% 
  ggplot(aes(NMDS1, NMDS2)) +
  geom_point(size = 2) +
  theme_classic()

# hay diferentes formas de incluir variables descriptivas a este plot
adicional_var2 <- field_data %>% 
  distinct(Locality, .keep_all = TRUE) %>% 
  arrange(Locality) %>% # el orden debe ser exactamente igual al de la matriz
  select(Locality, Department, ParamoComplex) 

data.scores2$Department = adicional_var2$Department #adicionar departamento
data.scores2$ParamoComplex = adicional_var2$ParamoComplex #adicionar complejo de paramo

data.scores2 %>% 
  ggplot(aes(x=NMDS1, y=NMDS2, color = ParamoComplex))+
  geom_point(size=2) +
  theme(panel.background = element_blank(), panel.border = element_rect(color="black", fill=NA, size = 1.2))+
  labs(color="Complejo de Paramo")+
  stat_ellipse(alpha = 0.4)
ggsave("figuras/nmds2_complejos_matriz de 85 comunidades.pdf", width = 10, height = 6)

# sin embargo esta opcion implica que todas las observaciones en ese complejo de paramo, 
# estan definidas bajo un mismo cluster "climatico", lo cual sabemos que no es cierto

field_data %>% 
  group_by(Locality, clusters) %>% 
  summarise(n()) %>% 
  pivot_wider(names_from = clusters, values_from = `n()`)

# por esta razon se propone definir las comunidades y unidades de comparacion combinando ambos criterios.
# es decir, Localidades definidas en la base de datos (85) y sus respectivos clusters.
# en muchos casos toda la informacion de una localidad esta dentro de un solo cluster, pero en otros esta localidad
# presenta coordenadas definidas en dos clusters, por lo cual esas comunidades de aves podrian ser diferentes debido a las diferencias climaticas


# NMDS 3 - El tercer analisis se hara a nivel de localidades y clusters.
# en lugar de usar las 483 localidades definidas por coordenadas unicas, o las 85 localidades definidas en la base de datos.
# se creara una matriz de abundancia para las localidades de la base de datos y sus clusters.

field_data <- field_data %>% 
  mutate(cluster_loc = paste(clusters, Locality, sep = "_")) #crear nueva variable que combina el cluster y la localidad

field_data %>% distinct(cluster_loc) #116 localidades (i.e comunidades)

abundance3 <- field_data %>% 
  group_by(Species, cluster_loc) %>% 
  summarize(abundance_sp = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = abundance_sp, values_fill = list(abundance_sp = 0)) %>%
  arrange(cluster_loc)

abundance3 # nueva matriz de abundancias de 116 comunidades y 179 especies

# transformar a matriz para usar la funcion metaMDS
data_m3 <- as.matrix(abundance3[, 2:180])

# NMDS3
set.seed(100)
nmds3 <- metaMDS(data_m3, k=2, distance="bray", trymax=100)

nmds3 
plot(nmds3)
stressplot(nmds3)

# los plots diagnosticos muestran que una comunidad es poco informativa,
# la cual se identifico como la numero 107 que es "4_Santander: Concepcion: Vda. Aguabri, Jurado"
field_data %>% 
  filter(cluster_loc == "4_Santander: Concepcion: Vda. Aguabri, Jurado") 

# como se observa esta comunidad tiene una sola especie lo cual es poco informativo para el nmds

# Por esta razon se volvera a definir la matriz de abundancia3 excluyendo esta comunidad, y se correra de nuevo el nmds3

abundance3 <- abundance3 %>% 
  filter(!cluster_loc == "4_Santander: Concepcion: Vda. Aguabri, Jurado") # nueva matriz de abundancias de 115 comunidades y 179 especies

# NMDS3
data_m3 <- as.matrix(abundance3[, 2:180])
set.seed(100)
nmds3 <- metaMDS(data_m3, k=2, distance="bray", trymax=100)

nmds3 
plot(nmds3)
stressplot(nmds3)

# extraer NMDS scores (x and y coordinates) para visualizar los resultados
data.scores3 <- as.data.frame(scores(nmds3)$sites)

data.scores3 %>% 
  ggplot(aes(NMDS1, NMDS2)) +
  geom_point(size = 2) +
  theme_classic()

# incluir variables descriptivas
adicional_var3 <- field_data %>% 
  filter(!cluster_loc == "4_Santander: Concepcion: Vda. Aguabri, Jurado") %>% 
  distinct(cluster_loc, .keep_all = TRUE) %>% 
  arrange(cluster_loc) %>% # el orden debe ser exactamente igual al de la matriz
  select(cluster_loc, Locality, Department, ParamoComplex, clusters) 

data.scores3$clusters = adicional_var3$clusters
data.scores3$cluster_loc = adicional_var3$cluster_loc
data.scores3$Department = adicional_var3$Department
data.scores3$ParamoComplex = adicional_var3$ParamoComplex

data.scores3 %>% 
  ggplot(aes(x=NMDS1, y=NMDS2, color = as_factor(clusters)))+
  geom_point(size=2) +
  theme(panel.background = element_blank(), panel.border = element_rect(color="black", fill=NA, size = 1.2))+
  labs(color="Clusters")+
  stat_ellipse(alpha = 0.40)
ggsave("figuras/nmds3_clusters_matriz de 115 comunidades.pdf", width = 10, height = 6)

data.scores3 %>% 
  ggplot(aes(x=NMDS1, y=NMDS2, color = ParamoComplex))+
  geom_point(size=2) +
  theme(panel.background = element_blank(), panel.border = element_rect(color="black", fill=NA, size = 1.2))+
  labs(color="Complejo de Paramo")+
  stat_ellipse(alpha = 0.40)
ggsave("figuras/nmds3_por_complejos_matriz de 85 comunidades.pdf", width = 10, height = 6)
