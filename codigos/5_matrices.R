# April 6, 2022

# Codigo para incorporar la informacion de clusters y nombres unicos de cada localidad
# en la base de datos "field" con los datos de capturas de especies.

# cargar paquetes
suppressPackageStartupMessages(library(tidyverse))

#cargar datos
field <- read_csv("sub_datos/paramo_field.csv")
coordinates <- read_csv("sub_datos/coordinates_clusters.csv")

### PARTE 1 ### Unir bases de datos parciales

# para unir la informacion de la base de datos de coordenadas a la base de datos 
# de capturas, usaremos "left_join", esta funcion preserva todas las filas de la 
# base de datos de capturas y adiciona las nuevas columnas de la base de datos de
# coordenadas.

field_data <- field %>% 
  left_join(coordinates) 

# exportar
write_csv(field_data, "sub_datos/paramo_field_and_clusters.csv")

#la union se hizo por las columnas presentes en las dos bases de datos, las
# cuales son consistentes

### PARTE 2 ### Explorar datos por localidad unica

# primero debemos explorar cuantos registros y especies hay por localidad

summary_by_locality <- field_data %>% 
  group_by(unique_locality) %>% 
  summarise(total_registros = n(),
            total_especies = n_distinct(Species)) %>% 
  arrange(total_registros) 

# plot del numero de registros por localidad
summary_by_locality %>% 
  ggplot(aes(reorder(unique_locality, -total_registros), total_registros)) +
  geom_bar(stat = "identity")

#plot del numero de especies por localidad
summary_by_locality %>% 
  ggplot(aes(reorder(unique_locality, -total_especies), total_especies)) +
  geom_bar(stat = "identity")

# registros y especies por cluster
field_data %>% 
  group_by(clusters) %>% 
  summarise(total_registros = n(),
            total_especies = n_distinct(Species)) %>% 
  arrange(total_registros)

# Mas de 90 localidades tienen 1 solo registro y una sola especie. 
# Esto generaria que en una matriz de especies por localidad existan muchos 
# ceros. 
# Debemos repensar nuevamente las suposiciones de la base de datos en cuanto a muestreo!!!


### PARTE 3 ### Crear matrices que necesita Juan Carlos para analisis siguientes

### 3.1 Matriz ambiental: Localidades y variables ambientales

coordinates_variables <- read_csv("sub_datos/coordinates_wc_19variables.csv")
coordinates_id <- read_csv("sub_datos/coordinates_clusters.csv")

coordinates_combined <- coordinates_variables %>% 
                    left_join(coordinates_id)

coordinates_combined <- coordinates_combined %>% 
  select(27, 7:25) %>% 
  arrange(unique_locality) %>% 
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

write_csv(coordinates_combined, "sub_datos/matriz_ambiental.csv")

### 3.2 Matriz de abundancias: Localidades y especies
data <- read_csv("sub_datos/paramo_field_and_clusters.csv")

abundance <- data %>% 
  group_by(Species, unique_locality) %>% 
  summarize(abundance_sp = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Species, values_from = abundance_sp, values_fill = list(abundance_sp = 0)) %>%
  arrange(unique_locality)

write_csv(abundance, "sub_datos/matriz_abundancias.csv")

### 3.3 Matriz de rasgos: Especies y promedio de rasgos
rasgos <- read_csv("sub_datos/paramo_field_and_clusters.csv")

rasgos_means <- rasgos %>% 
  select(22:37) %>% 
  group_by(Species) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE),
                   .names = "mean_{.col}"))

write_csv(rasgos_means, "sub_datos/matriz_promedio_rasgos.csv")

