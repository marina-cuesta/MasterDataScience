############################
### librerias necesarias ###
############################

library(ggplot2)
#install.packages("tsne")
library(tsne) 



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



############
### tsne ###
############

# tsne estandarizando
?tsne
tsne_results <- tsne(scale(iris[,1:4]),k=2,perplexity=20)
tsne_results


##############################
### visualizacion del tsne ###
##############################

ggplot(as.data.frame(tsne_results[,1:2])) +
  geom_point(aes(x=tsne_results[,1],y=tsne_results[,2]), col=colors()[c(55,150,300)][iris$Species])

