#Aplicación del Analisis RLQ y de la cuarta esquina
#Cargar paquetes
suppressPackageStartupMessages(library(tidyverse))
library(ade4)
library(dplyr)
library(readxl)

#Prueba con datos de Ste´hane Dray (2013)
#cargar datos
data("aravo")
#Dimenciones
dim(aravo$spe)
dim(aravo$traits)
dim(aravo$env)

#Analisis individuales para cada matriz
afcL.aravo <- dudi.coa(aravo$spe, scannf = F)
acpR.aravo <- dudi.hillsmith(aravo$env, row.w = afcL.aravo$lw, scannf = FALSE)
acpQ.aravo <- dudi.pca(aravo$traits, row.w = afcL.aravo$cw,
                       scannf = FALSE)
rlq.aravo <- rlq(acpR.aravo, afcL.aravo, acpQ.aravo,
                 scannf = FALSE)

#Comparaciones con los analisis de cada matriz
summary(rlq.aravo)



#Cargar Datos
R <- read_csv("sub_datos/matriz_ambiental.csv")   #Matriz ambiental
L <- read_csv("sub_datos/matriz_abundancias.csv") #Matriz de abundancias
Q <- read_csv("sub_datos/Matriz_Promedio_rasgos_completaa.csv") #Matriz de rasgos

#Analisis individuales para cada matriz
R2 <- column_to_rownames(R, var = "unique_locality")
L2 <- column_to_rownames(L, var = "unique_locality")
Q2 <- column_to_rownames(Q, var = "Species")

R3<- abs(R2)
L3<- abs(L2)
Q3<- abs(Q2)


afcL <- dudi.coa(L3, scannf = FALSE)
acpR <- dudi.hillsmith(R3, row.w = afcL$lw, scannf = F)

#pcaR <- dudi.pca(R2, row.w = afcL$cw, scannf = FALSE)

acpQ <- dudi.pca(Q3, row.w = afcL$cw, scannf = FALSE)
rlqras <- rlq(acpR, afcL, acpQ, scannf = FALSE)

#Plots para cada analisis en un solo plot 

plot(rlqras)



#Plots separados del rlqras
par(mfrow = c(1, 3))
s.arrow(rlqras$l1) #Combinación lineal de coeficintes
s.arrow(rlqras$c1) #coeficientes
s.label(rlqras$lQ, boxes = FALSE) #species scores


#Comparaciones con los analisis de cada matriz
summary(rlqras)


#ANALISIS DE LA 4 ESQUINA
nrepett <- 1000
four.comb <- fourthcorner(R3, L3, Q3, modeltype = 6, p.adjust.method.G = "none", p.adjust.method.D = "none", nrepet = nrepett)


plot(four.comb, alpha = 0.05, stat = "D2")
ggsave("figuras/fourcor.pdf") #guardamos

#Ahora con p-valores ajustados
four.comb.adj <- p.adjust.4thcorner(four.comb, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")
plot(four.comb.adj, alpha = 0.05, stat = "D2")
ggsave("figuras/fourcoradjust.pdf") #guardamos


#combinando ambos enfoques
testrlq <- randtest(rlqras, modeltype = 6, nrepet = nrepett)
testrlq
plot(testrlq)
ggsave("figuras/testrql.pdf") #guardamos

#Inercia total del analis RLQ = (Srlq)
Srlq <- fourthcorner2(R3, L3, Q3, modeltype = 6, p.adjust.method.G = "fdr", nrepet = nrepett)
Srlq$trRLQ
plot(four.comb.adj, x.rlq = rlqras, alpha = 0.05, stat = "D2", type = "biplot")
ggsave("figuras/totalinertia.pdf") #guardamos

#Otro enfoque para probar directamente vinculos entre los ejes RLQ y los rasgos o variables ambientales
testQaxes.comb <- fourthcorner.rlq(rlqras, modeltype = 6, typetest = "Q.axes", nrepet = nrepett, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")
testRaxes.comb <- fourthcorner.rlq(rlqras, modeltype = 6, typetest = "R.axes", nrepet = nrepett, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")
print(testQaxes.comb, stat = "D")
print(testRaxes.comb, stat = "D")

#Los resultados se pueden representar cn tablas de colores
par(mfrow = c(1,2))
plot(testQaxes.comb, alpha = 0.05, type = "table", stat = "D2")
plot(testRaxes.comb, alpha = 0.05, type = "table", stat = "D2")

#Mapa factorial, relaciones significativas con el primer eje son azules
#con el segundo son naranjas y con los dos son verdes
par(mfrow = c(1, 2))
plot(testQaxes.comb, alpha = 0.05, type = "biplot", stat = "D2", col = c("black", "blue", "orange", "green"))
plot(testRaxes.comb, alpha = 0.05, type = "biplot", stat = "D2", col = c("black", "blue", "orange", "green"))

#segun los analisis de la cuarta esquina y el acercamiento entre los dos enfoques
#no hay realciones significantes entre los rasgos de las especies y las variabes ambientales, 
#pero es necerario revisar bien lo ontenido antes de ajustar el valor p y en el analisis RLQ.
