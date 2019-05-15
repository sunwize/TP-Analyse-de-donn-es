library(ggplot2)
library("gridExtra")
library(scatterplot3d)
library(rgl)

#1)
Database <- read.table("/amuhome/c17022552/Bureau/ANADONNEES/data1TP2.txt", header=TRUE)
summary(Database)

data <- subset(Database)
nuage3d <- scatterplot3d(data)

#2)
B <- scale(x = data, center = TRUE, scale = FALSE)

V <- cov(x = data, y = NULL)

#3)
x <- eigen(V)
x$values # Valeurs propres
x$vectors # Vecteurs propres

#4) Ordre des axes principaux : stature > poids > taille
pca <- prcomp(V)

print( 100 * pca$sdev^2 / sum(pca$sdev^2) ) # Proportion de variance de chaque composante
print( sum(100 * (pca$sdev^2)[1] / sum(pca$sdev^2)) ) # Variance totale de la colonne Poids
plot(pca)

#5)
dim(B)
dim(x$vectors)
C <- B %*% x$vectors
C
princomp(data)$scores

#6)
pca
pca[2]$rotation[1,1]
pca[2]$rotation[2,1]
pca[2]$rotation[3,1]

x <- c(pca[2]$rotation[1,1], 0)
y <- c(pca[2]$rotation[2,1], 0)
z <- c(pca[2]$rotation[3,1], 0)

plot3d(x, y, z, type = "l")
plot3d(x = C[,1], y = C[,2], z = C[,3], type = "p")

#7)
plot(C[,1], C[,2])

#8) Les résultats sont cohérents

