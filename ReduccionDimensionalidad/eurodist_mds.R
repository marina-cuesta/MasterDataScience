############################
### librerias necesarias ###
############################

library(ggplot2)
library(MASS)


########################
### eurodist dataset ###
########################

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/eurodist

eurodist
# los datos son una matriz de distancia

#dimensiones
dim(as.matrix(eurodist))


###################
### classic mds ###
###################

?cmdscale
cmds = cmdscale(eurodist,k=2)
cmds

# visualizacion de las coordenadas obtenidas
ggplot(as.data.frame(cmds), aes(x= cmds[,1], y= cmds[,2], label=rownames(as.matrix(eurodist))))+
  geom_point() +geom_text(hjust=0, vjust=0)



######################
### non-metric mds ###
######################

?isoMDS
mds=isoMDS(eurodist,k=2)

## coordenadas obtenidas
mds$points

# visualizacion de las coordenadas obtenidas
ggplot(as.data.frame(mds$points), aes(x= mds$points[,1], y= mds$points[,2], label=rownames(as.matrix(eurodist))))+
  geom_point() +geom_text(hjust=0, vjust=0)
