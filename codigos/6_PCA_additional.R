### 20-Mayo-2022

# Codigo para producir PCA
#- uno para las variables climaticas por localidad (igual al PCA en el codigo 4_multivariate.R)
#- uno para los rasgos funcionales por especie

# cargar paquetes
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)

# PCA 1_variables ambientales de WorldClim y localidades ------

# cargar datos
envdata <- read_csv("sub_datos/coordinates_wc_19variables.csv")

# renombrar variables 
envdata <- envdata %>% 
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

# construir PCA
res.pca <- PCA(envdata[,7:25], scale.unit = TRUE, ncp = 5, graph = F) # mantener los 5 primeros componentes y estandarizar variables

### extraer diferentes parametros y graficas del PCA 1

# grafica de los "eigenvalues"
eigenvalues_plot <- fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
ggsave(eigenvalues_plot, "figuras/PCA1_eigenvalues_plot.pdf", width = 8, height = 6)

# grafica de la correlacion entre variables
correlation_plot <- plot(res.pca, choix = "var")
ggsave(correlation_plot, "figuras/PCA1_correlation_plot.pdf", width = 8, height = 6)

# parametros de las variables
var <- get_pca_var(res.pca)

# tabla de la calidad de representacion de las variables
table1 <- data.frame(var$cos2)
write.csv(table1, "tablas/PCA1_quality_of_representation_variables.csv")

# grafica de la calidad de representacion de las variables var$cos2
pdf("figuras/PCA1_quality_of_representation_plot.pdf")
corrplot(var$cos2, is.corr=FALSE)
dev.off()

# tabla de la contribucion de las variables
table2 <- data.frame(var$contrib)
write.csv(table2, "tablas/PCA1_contribution_variables.csv")

# grafica de las contribuciones de variables a los dos primeros componentes
pdf("figuras/PCA1_contribution_of_variables_dim1-2_plot.pdf")
corrplot(var$contrib[,1:2], is.corr=FALSE)
dev.off()

# Grafica de correlacion de variables con su contribucion a los primeros dos componentes
pdf("figuras/PCA1_correlation_plot_by_contribution.pdf")
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping
dev.off()

# Tabla de correlaciones entre variables para el primer componente
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)

table3 <- data.frame(res.desc$Dim.1)
write.csv(table3, "tablas/PCA1_correlation_variables_dimension1.csv")

# Tabla de correlaciones entre variables para el segundo componente
table4 <- data.frame(res.desc$Dim.2)
write.csv(table4, "tablas/PCA1_correlation_variables_dimension2.csv")

# grafica "biplot" de las variables y las localidades
pdf("figuras/PCA1_biplot_localides_and_variables.pdf")
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
dev.off()

# exportar todos los resultados del PCA1
write.infile(res.pca, "tablas/PCA1_all_results.csv", sep = ";")


# PCA 2_rasgos funcionales y especies ------

# cargar datos
rasgos <- read_csv("sub_datos/matriz_promedio_rasgos_completa.csv")

# excluir las 5 especies identificadas con muchos valores de NA
rasgos <- rasgos %>% 
  filter(!Species %in% c("Cyphorhinus thoracicus", "Elaenia albiceps", "Grallaricula cucullata",
                        "Mecocerculus minor", "Oxypogon stubelii" ))

# la especie Empidonax virescens no tiene dato de mean_WingLength, por lo cual este valor se tomara de AVONET (Wing.Length = 71)
rasgos <- rasgos %>% 
  mutate(mean_wingLength = case_when(
  Species == "Empidonax virescens" ~ 71,
  TRUE ~ mean_wingLength
))

# construir PCA
res.pca2 <- PCA(rasgos[,2:11], scale.unit = TRUE, ncp = 5, graph = F) # mantener los 5 primeros componentes y estandarizar variables

### extraer diferentes parametros y graficas del PCA 2

# grafica de los "eigenvalues"
fviz_eig(res.pca2, addlabels = TRUE, ylim = c(0, 50))
ggsave("figuras/PCA2_rasgos_eigenvalues_plot.pdf", width = 8, height = 6)

# grafica de la correlacion entre variables
plot(res.pca2, choix = "var")
ggsave("figuras/PCA2_rasgos_correlation_plot.pdf", width = 8, height = 6)

# parametros de las variables
var2 <- get_pca_var(res.pca2)

# tabla de la calidad de representacion de las variables
table1 <- data.frame(var2$cos2)
write.csv(table1, "tablas/PCA2_rasgos_quality_of_representation_variables.csv")

# grafica de la calidad de representacion de las variables var$cos2
pdf("figuras/PCA2_rasgos_quality_of_representation_plot.pdf")
corrplot(var2$cos2, is.corr=FALSE)
dev.off()

# tabla de la contribucion de las variables
table2 <- data.frame(var2$contrib)
write.csv(table2, "tablas/PCA2_rasgos_contribution_variables.csv")

# grafica de las contribuciones de variables a los dos primeros componentes
pdf("figuras/PCA2_rasgos_contribution_of_variables_dim1-2_plot.pdf")
corrplot(var2$contrib[,1:2], is.corr=FALSE)
dev.off()

# Grafica de correlacion de variables con su contribucion a los primeros dos componentes
pdf("figuras/PCA2_rasgos_correlation_plot_by_contribution.pdf")
fviz_pca_var(res.pca2, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) # Avoid text overlapping
dev.off()

# Tabla de correlaciones entre variables para el primer componente
res.desc <- dimdesc(res.pca2, axes = c(1,2), proba = 0.05)

table3 <- data.frame(res.desc$Dim.1)
write.csv(table3, "tablas/PCA2_rasgos_correlation_variables_dimension1.csv")

# Tabla de correlaciones entre variables para el segundo componente
table4 <- data.frame(res.desc$Dim.2)
write.csv(table4, "tablas/PCA2_rasgos_correlation_variables_dimension2.csv")

# grafica "biplot" de los rasgos y las especies
pdf("figuras/PCA2_rasgos_biplot_rasgos_especies.pdf")
fviz_pca_biplot(res.pca2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
dev.off()

# exportar todos los resultados del PCA2
write.infile(res.pca2, "tablas/PCA2_rasgos_all_results.csv", sep = ";")


