############################
### librerias necesarias ###
############################

library(ggplot2)


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



##################################################
### matriz de distancia euclidea de datos iris ###
##################################################

matriz_distancia=dist(iris[,1:4])


###################
### mds clasico ###
###################

# mds sobre la matriz de distancas
?cmdscale
mds = cmdscale(matriz_distancia,k=3,eig=T)
mds


# coordenadas obtenidas
mds$points



#############################
### visualizacion del mds ###
#############################


# visualizacion de las coordenadas obtenidas
ggplot(as.data.frame(mds$points[,1:2])) +
  geom_point(aes(x=mds$points[,1],y=mds$points[,2]), col=colors()[c(55,150,300)][iris$Species])
