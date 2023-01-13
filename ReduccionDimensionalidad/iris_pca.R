############################
### librerias necesarias ###
############################

library(ggplot2)

library(devtools)

# install_github("vqv/ggbiplot")
library(ggbiplot) 

#install.packages("factoextra")
library(factoextra)  # Package for PCA visualization



####################
### iris dataset ###
####################

# cargamos los datos
data(iris)

# caracteristicas de los datos
dim(iris)
str(iris)

# resumen de los datos
summary(iris)



###########
### pca ###
###########

# pca estandarizando los datos y seleccionando las variables numericas
pca <- prcomp(scale(iris[,1:4]))

# resumen de pca
summary(pca)

# autovalores del pca
eig_val <- get_eigenvalue(pca)
eig_val

# matriz de transformacion
pca$rotation

# componentes principales
pca$x



#############################
### visualizacion del pca ###
#############################

# varianza explicada por las componentes
plot(pca,type = "l",
     main="Variance explained by PCA"
)


# visualizacion de las componentes obtenidas: PC1 vs PC2
ggplot(as.data.frame(pca$x[,1:2])) +
  geom_point(aes(x=PC1,y=PC2), col=colors()[c(55,150,300)][iris$Species])


#  biplot: grafico de puntos y variables conjuntamente
biplot_pca <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = iris[,5], ellipse = F,
              circle = FALSE)
print(biplot_pca)
