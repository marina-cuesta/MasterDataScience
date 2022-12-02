library(dplyr)
library(RKEEL)


##############################
####    mtcars DATA SET   ####
##############################

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
# Variable respuesta: mpg

library(car)
# Llamamos a los datos
data(mtcars)
# Mostramos datos
head(mtcars)
# Dimensiones
dim(mtcars)
# Variables e info
str(mtcars)
summary(mtcars)
# SPLOM
pairs(mtcars, panel = panel.smooth, main = "mtcars data")



#################################
####    state.x77 DATA SET   ####
#################################

# https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/state.html
# Variable respuesta: Life Exp

library(datasets)
# Llamamos a los datos
data(state)
ls()
state.x77=as.data.frame(state.x77)
# Mostramos datos
head(state.x77)
# Dimensiones
dim(state.x77)
# Variables e info
str(state.x77)
summary(state.x77)
# SPLOM
pairs(state.x77, panel = panel.smooth, main = "state.x77 data")



##################################
####    airquality DATA SET   ####
##################################

# https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/airquality
# Variable respuesta: Ozone

library(datasets)
# Llamamos a los datos
data(airquality)
# Mostramos datos
head(airquality)
# Dimensiones
dim(airquality)
# Variables e info
str(airquality)
summary(airquality)
# SPLOM
pairs(airquality, panel = panel.smooth, main = "airquality data")



###############################
####    abalone DATA SET   ####
###############################

# https://rdrr.io/rforge/AppliedPredictiveModeling/man/abalone.html
# Variable respuesta: abalone

# install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
# Llamamos a los datos
data(abalone)
# Mostramos datos
head(abalone)
# Dimensiones
dim(abalone)
# Variables e info
str(abalone)
summary(abalone)
# SPLOM
pairs(abalone, panel = panel.smooth, main = "abalone data")



################################
####    concrete DATA SET   ####
################################

# https://rdrr.io/rforge/AppliedPredictiveModeling/man/concrete.html
# https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength
# Variable respuesta: CompressiveStrength

# install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
# Llamamos a los datos
data(concrete)
# Mostramos datos
head(concrete)
# Dimensiones
dim(concrete)
# Variables e info
str(concrete)
summary(concrete)
# SPLOM
pairs(concrete, panel = panel.smooth, main = "concrete data")



#############################################
####    communities.and.crime DATA SET   ####
#############################################

# https://rdrr.io/cran/fairml/man/communities.and.crime.html
# Variable respuesta: ViolentCrimesPerPop
# Hay variables sensibles (mirar documentacion)

# install.packages("fairml")
library(fairml)
# Llamamos a los datos
data(communities.and.crime)
# Mostramos datos
head(communities.and.crime)
# Dimensiones
dim(communities.and.crime)
# Variables e info
str(communities.and.crime)
summary(communities.and.crime)
# SPLOM
pairs(communities.and.crime, panel = panel.smooth, main = "communities.and.crime data")



##################################
####    california DATA SET   ####
##################################

# https://sci2s.ugr.es/keel/dataset.php?cod=83
# Variable respuesta: MedianHouseValue

# Leemos a los datos
file_name=paste0(getwd(),"/RegresionLineal/data/california.dat")
california=read.keel(file_name)

# Mostramos datos
head(california)
# Dimensiones
dim(california) # submuestrear si es necesario
# Variables e info
str(california)
# Pasamos variable MedianHouseValue a numerica
california = california %>% 
  mutate(MedianHouseValue=as.numeric(as.character(MedianHouseValue)))
summary(california)
# SPLOM
pairs(california, panel = panel.smooth, main = "california data")



################################
####    mortgage DATA SET   ####
################################

# https://sci2s.ugr.es/keel/dataset.php?cod=43
# Variable respuesta: X30Y.CMortgageRate

# Leemos a los datos
file_name=paste0(getwd(),"/RegresionLineal/data/mortgage.dat")
mortgage=read.keel(file_name)

# Mostramos datos
head(mortgage)
# Dimensiones
dim(mortgage) # submuestrear si es necesario!
# Variables e info
str(mortgage)
# Pasamos variable MedianHouseValue a numerica
mortgage = mortgage %>% 
  mutate(X30Y.CMortgageRate=as.numeric(as.character(X30Y.CMortgageRate)))
summary(mortgage)
# SPLOM
pairs(mortgage, panel = panel.smooth, main = "mortgage data")



#################################
####    SeoulBike DATA SET   ####
#################################

# https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand
# Variable respuesta: Rented Bike Count

# Leemos a los datos
file_name=paste0(getwd(),"/RegresionLineal/data/SeoulBikeData.csv")
SeoulBike=read.table(file_name,sep=",")

# Mostramos datos
head(SeoulBike)

# La primera fila son los nombres de las columnas
colnames(SeoulBike)=as.character(SeoulBike[1,])
head(SeoulBike)
# aun asi los nombres de las variables son poco manejables. Los cambiamos
colnames(SeoulBike)= c("date","rented_bike_count", "hour", "temperature" , "humidity","wind_speed" ,
                       "visibility", "dew_point_temperature","solar_radiation","rainfall","snowfall",
                       "seasons" ,"holiday","functioning_day")
# Eliminamos la primera fila
SeoulBike=SeoulBike[-1,]

# Mostramos datos
head(SeoulBike)

# Dimensiones
dim(SeoulBike) 
# Variables e info
str(SeoulBike)
# Cambiamos los tipos de variable
library(lubridate)
SeoulBike = SeoulBike %>% 
  mutate(date=as.Date(date, format = "%d/%m/%Y")) %>% 
  mutate(across(-c(date,seasons,holiday,functioning_day),as.numeric)) %>% 
  mutate(across(c(seasons,holiday,functioning_day),as.factor))
str(SeoulBike)

# resumen de la tabla
summary(SeoulBike)
# SPLOM
pairs(SeoulBike, panel = panel.smooth, main = "SeoulBike data")



####################################
####    stock prices DATA SET   ####
####################################

# https://sci2s.ugr.es/keel/dataset.php?cod=77
# Variable respuesta: Company10


# Leemos a los datos
# PARA HACER EN CLASE

