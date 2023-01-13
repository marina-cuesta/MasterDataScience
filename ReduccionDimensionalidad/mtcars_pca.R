############################
### librerias necesarias ###
############################

library(ggplot2)

library(devtools)

# install_github("vqv/ggbiplot")
library(ggbiplot) 

#install.packages("factoextra")
library(factoextra)  # Package for PCA visualization



######################
### mtcars dataset ###
######################

# cargamos los datos
data(mtcars)

# caracteristicas de los datos
dim(mtcars)
str(mtcars)
View(mtcars) # la variables vs y am son binarias!!

# resumen de los datos
summary(mtcars)



###########
### pca ###
###########

# pca estandarizando los datos y seleccionando las variables numericas
pca <- prcomp(scale(mtcars[,c(1:7,10:11)]))

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
ggplot(as.data.frame(pca$x[,1:2]), aes(x= PC1, y= PC2, label=rownames(mtcars)))+
  geom_point() +geom_text(hjust=0, vjust=0)


#  biplot: grafico de puntos y variables conjuntamente
biplot_pca <- ggbiplot(pca, labels=rownames(mtcars))
print(biplot_pca)

