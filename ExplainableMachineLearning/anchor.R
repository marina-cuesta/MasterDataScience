######################
######################
######  ANCHOR  ######
######################
######################


####################################
######  INSTALAMOS PAQUETES   ######
####################################

## lista de paquetes instalados
installed_packages=installed.packages()

## Lista de paquetes a usar
list.of.packages = c("mlbench",# para los datos
                     "dplyr",# para tratamiento de datos
                     "devtools",# Para instalar paquete
                     "mlr" , #para modelo
                     "caret") #para confusion matrix

## paquetes a usar que no estan instalados
new.packages = list.of.packages[!(list.of.packages %in% installed_packages)]

## instalamos esos paquetes si hay alguno
if(length(new.packages)) install.packages(new.packages)


## llamamos a los paquetes
for (package in list.of.packages) {
  library(package, character.only=T)
}


## intalamos el paquete anchorsOnR desde R
# devtools::install_github("viadee/anchorsOnR")
library(anchors)



####################################
######   OBTENEMOS DATOS      ######
####################################

## Leemos los datos Breast Cancer
# https://www.rdocumentation.org/packages/mlbench/versions/2.1-3/topics/BreastCancer
data(BreastCancer)
datos=BreastCancer
dim(datos)

## Vistazo a los datos
str(datos)
head(datos)
summary(datos)

## La variable ID es irrelevante. La eliminamos
datos= datos %>% select(-Id)

## Eliminamos valores faltantes del dataset
datos_tratados = na.omit(datos)
str(datos_tratados)



####################################################
######   PREPARACION DE DATOS PARA MODELO     ######
####################################################

## Otenemos las variables explicativas X de los datos
datosX = datos_tratados %>% select(-Class)

## Las pasamos de factor a numerico
datosX = as.data.frame(lapply(datosX, function(x)  as.numeric(x)))

## Estandarizamos las variables X
max_datos = apply(datosX, 2, max)
min_datos = apply(datosX, 2, min)
datosX_std = as.data.frame(scale(datosX,center = min_datos,scale = max_datos - min_datos))

## Otenemos la variable respuesta Y de los datos
cancer=datos_tratados$Class

## Juntamos datos X y datos Y para tener los datos en un solo data frame para el modelo
datos_modelo=cbind(datosX_std,cancer)



####################################################################
######   APLICAMOS UN MODELO DE RED NEURONAL: CAJA NEGRA      ######
####################################################################

## Dividimos los datos en un conjunto de entrenamiento y conjunto de test 
# fijamos la semilla de la aleatoriedad de la particion
set.seed(12345) 
# indices de los datos que seran de entrenamiento
index_train = sample(1:nrow(datos_modelo),round(0.90*nrow(datos_modelo)))
# Datos de entrenamiento y test:
train_data = datos_modelo[index_train,] # datos de entrenamiento
test_data = datos_modelo[-index_train,] # datos de test


# La tarea es predecir la variable cancer
task = makeClassifTask(data = train_data, target = "cancer", id = "breastcancer")

# Para ello iniciamos un learner de red neuronal artificial
lrn=makeLearner("classif.nnet", predict.type = "response")

# Entrenamos la red neuronal en el conjunto de entrenamiento
# fijamos la semilla 
set.seed(12345) 
model = mlr::train(learner = lrn, task = task)


## Obtenemos predicciones de la red neuronal sobre los datos tests
preds_model = predict(model, newdata = test_data)
 # la prediccion es un objeto con varios atributos
preds_model
 # nos quedamos con "response" que es la prediccion del modelo
preds=preds_model$data$response


## Tabla de contingencia de predicciones vs valores reales
preds_table = table(cancer[-index_train], preds)
colnames(preds_table)= c("benign", "malignant")
row.names(preds_table)= c("benign", "malignant")
preds_table


## Valores performance del modelo
confusionMatrix(preds_table, mode = "prec_recall")



###############################################################################
######   EXPLICABILIDAD DEL MODELO MEDIANTE MODELO SUSTITUTO ANCHOR      ######
###############################################################################

## Usamos la funcion anchors() para entrenar los modelos locales interpretables en nuestros datos train
explainer = anchors(datos_modelo, model, target = "cancer")

## ix a explicar row.names(test_data)
ix_explicar=c(45,138,560)

## Calculamos sus explicaciones
explanations = explain(datos_modelo[ix_explicar,], explainer)

## Las imprimimos por pantalla
printExplanations(explainer, explanations)


## OJO! VEMOS QUE CON VARIABLES CONTINUAS NO TIENEN SENTIDO LAS EXPLICACIONES 
 #      TENEMOS QUE DISCRETIZAR LAS VARIABLES

bins=list()
for (j in 1:(dim(datos_modelo)[2]-1)){
  bins[[j]] = list(cuts = seq(0,1,by=0.1))
}

## Usamos la funcion anchors() para entrenar los modelos locales interpretables en nuestros datos train
explainer = anchors(datos_modelo, model, target = "cancer", bins=bins)

## ix a explicar row.names(test_data)
ix_explicar=c(98) #c(45,138,560) #560, 611,50,98,

## Calculamos sus explicaciones
explanations = explain(datos_modelo[ix_explicar,], explainer)

## Las imprimimos por pantalla
printExplanations(explainer, explanations)
