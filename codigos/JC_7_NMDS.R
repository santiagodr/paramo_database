#ESCALAMIENTO MULTIDIMENCIONAL NO METRICO (NMDS)
#Cargar paquetes
install.packages("scatterplot3d")
suppressPackageStartupMessages(library(tidyverse))
library(scatterplot3d)
library(vegan)
library(kableExtra)
library(corrplot)
library(readxl)
library(dplyr)


#Cargar datos
L <- read_csv("sub_datos/matriz_abundancias.csv") #Matriz de abundancias
Q <- read_csv("sub_datos/Matriz_Promedio_rasgos_completaa.csv") #Matriz de rasgos
L2 <- column_to_rownames(L, var = "unique_locality")
Q2 <- column_to_rownames(Q, var = "Species")

#Analisis de disimilitud para NMDS o Hieralchical Clustering

dist <- vegdist(L3, method = "bray")

#Valor absoluto de los valores ya que hay algunos negativos 

scaletraits<- abs(Q2)


#Analisis NMDS
set.seed(0) #Para que los resultados no se brinden aleatorios
nmds1<- metaMDS(scaletraits)
plot(nmds1)
plot(nmds1, type = "n")
points(nmds1$points, pch = as.numeric(L), col = as.numeric(L$unique_locality), cex = 1.5)


ordiplot(nmds1, display = 'sites', type = 'n')
points(nmds1, col = as.numeric(L$unique_locality), pch = as.numeric(L$unique_locality))
for(i in unique(L$unique_locality)) ordihull(nmds1, groups = as.numeric(L$unique_locality, show.group = i, col = i), draw = 'polygon', label = T)



sum(is.na(L3))

#Otro enfoque 
nmds2<- metaMDS(scaletraits, k=2, trymax = 100)#con k se selecciona el # de dimenciones y con trymax se incrementan las iteraciones
stressplot(nmds2)#Dibuja un diagrama de Shepard donde las distancias de la ordenacion y e ajustefrente a las dimenciones 
#originales, si los puntos se separan muchode la linea no es bueno. 
nmds2 #Se calculan distancias de Bray-Curtis, stress <0.05 muy buena representación, <0.1 buena, <0.2 es adecuada y <03 la 
#representacion es un poco pobre, a mayor énfasis peor representación. 
plot(nmds2)#Representación
plot(nmds2, type = "t")#Representación con nombre de variables

#Se puede complicar la representación
ordiplot(nmds2, type = "n")
orditorp(nmds2, display = "species",col = "blue", air = 0.01)
orditorp(nmds2, display = "sites", cex = 1.2, air = 0.01)

#Con poligonos
treat=c(rep(""))

#Aqui, debido a que no supe como agrupar los rasgos y las especies no continue con las graficas. 



#NMDS de abundancia x localidad

Abun<- scale(L2, center = T,scale = T)
kable(head(Abun),format = "markdown")
L3 <- abs(Abun)
  
#Otro enfoque 
nmds3<- metaMDS(L3, k=2, trymax = 100)#con k se selecciona el # de dimenciones y con trymax se incrementan las iteraciones
stressplot(nmds3)#Dibuja un diagrama de Shepard donde las distancias de la ordenacion y e ajustefrente a las dimenciones 
#originales, si los puntos se separan muchode la linea no es bueno. 
ggsave("figuras/nmdsLstressplot.pdf") #guardamos
nmds3 #Se calculan distancias de Bray-Curtis, stress <0.05 muy buena representación, <0.1 buena, <0.2 es adecuada y <03 la 
#representacion es un poco pobre, a mayor énfasis peor representación. 
plot(nmds3)#Representación
ggsave("figuras/nmdsLrepresentacion.pdf") #guardamos
plot(nmds3, type = "t")#Representación con nombre de variables
ggsave("figuras/nmdsLrepconnames.pdf") #guardamos

#Se puede complicar la representación
ordiplot(nmds3, type = "n")
orditorp(nmds3, display = "species",col = "blue", air = 0.01)
orditorp(nmds3, display = "sites", cex = 1.2, air = 0.01)
ggsave("figuras/nmdsLrep2.pdf") #guardamos

#Con poligonos
treat=c(rep("Almorzadero", 15), rep("Chili_Barragan", 33), rep("Chiles-Cumbal", 6), rep("Chingaza", 11), rep("Citara", 3), 
        rep("Doña Juna_Chimayoy", 21), rep("Frontino_Urrao", 21), rep("Guantiva", 20), rep("las Hermosas", 24), rep("Iguaque-Merchan", 16), 
        rep("La Cocha-Patascoy", 15), rep("Los Nevados", 28), rep("Perija", 29), rep("Sumapaz-Cruz Verde", 196), rep("Sonsón", 17),
        rep("Sotara", 6), rep("Tota-Bijagual-Mamapacha", 18), rep("Tama", 4))
ordiplot(nmds3, type = "n")
ordihull(nmds3, groups = treat, draw = "polygon", col = "grey90", label = F)
orditorp(nmds3, display = "species", col = "red", air = 0.05)
orditorp(nmds3, display = "sites", col = c(rep("red", 15), rep("black", 33), rep("blue", 6), rep("maroon", 11), rep("green", 3), 
                                           rep("pink", 21), rep("aquamarine", 21), rep("brown", 20), rep("burlywood", 24), rep("orange", 16), 
                                           rep("coral", 15), rep("cyan", 28), rep("darkgoldenrod", 29), rep("darkseagreen", 196), rep("darkorchid", 17),
                                           rep("gold", 6), rep("indianred", 18), rep("khaki", 4)), air = 0.05, cex = 1.2)

ggsave("figuras/nmdsLpolygon.pdf") #guardamos

#Graficos de araña
ordiplot(nmds3, type = "n")
ordispider(nmds3, groups = treat,col = "black", label = F)
orditorp(nmds3, display = "species", col = "red", air = 0.05)
orditorp(nmds3, display = "sites", col = c(rep("red", 15), rep("black", 33), rep("blue", 6), rep("maroon", 11), rep("green", 3), 
                                           rep("pink", 21), rep("aquamarine", 21), rep("brown", 20), rep("burlywood", 24), rep("orange", 16), 
                                           rep("coral", 15), rep("cyan", 28), rep("darkgoldenrod", 29), rep("darkseagreen", 196), rep("darkorchid", 17),
                                           rep("gold", 6), rep("indianred", 18), rep("khaki", 4)), air = 0.05, cex = 1.2)
ggsave("figuras/nmdsLplotaraña.pdf") #guardamos


#Grafico de elipses
ordiplot(nmds3, type = "n")
ordiellipse(nmds3, groups = treat, col = "black", label = F)
orditorp(nmds3, display = "species", col = "red", air = 0.05)
orditorp(nmds3, display = "sites", col = c(rep("red", 15), rep("black", 33), rep("blue", 6), rep("maroon", 11), rep("green", 3), 
                                           rep("pink", 21), rep("aquamarine", 21), rep("brown", 20), rep("burlywood", 24), rep("orange", 16), 
                                           rep("coral", 15), rep("cyan", 28), rep("darkgoldenrod", 29), rep("darkseagreen", 196), rep("darkorchid", 17),
                                           rep("gold", 6), rep("indianred", 18), rep("khaki", 4)), air = 0.05, cex = 1.2)

ggsave("figuras/nmdsLelipsesplot.pdf") #guardamos





bgq2welevaciones <- runif(10, 0.5, 1.5)
ordisurf(nmds2, elevaciones, main = "", col = "forestgreen")
orditorp(nmds2, display = "species", col = "grey30", air = 0.1, cex = 1)
  
  