################################
################################
######  COUNTERFACTUALS   ######
################################
################################


####################################
######  INSTALAMOS PAQUETES   ######
####################################

## lista de paquetes instalados
installed_packages=installed.packages()

## Lista de paquetes a usar
list.of.packages = c("mlbench",# para los datos
                     "dplyr", # para tratamiento de datos
                     "xgboost", # para el modelo
                     "caret", # para la confusion matrix
                     "SHAPforxgboost", # para los valores SHAP 
                     "plotly") # para dibujar


## paquetes a usar que no estan instalados
new.packages = list.of.packages[!(list.of.packages %in% installed_packages)]

## instalamos esos paquetes si hay alguno
if(length(new.packages)) install.packages(new.packages)


## llamamos a los paquetes
for (package in list.of.packages) {
  library(package, character.only=T)
}



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


############################################################
######   PREPARACION DE DATOS PARA MODELO XGBOOST     ######
############################################################

## Otenemos las variables explicativas X de los datos
datosX = datos_tratados %>% select(-Class)

## Las pasamos de factor a numerico
datosX = as.data.frame(lapply(datosX, function(x)  as.numeric(x)))

## Otenemos la variable respuesta Y de los datos
# valor 0: benigno
# valor 1: maligno
cancer=as.numeric(datos_tratados$Class)-1

## Dividimos los datos en un conjunto de entrenamiento y conjunto de test 
# fijamos la semilla de la aleatoriedad de la particion
set.seed(1234) 
# indices de los datos que seran de entrenamiento
index_train = sample(1:nrow(datosX),round(0.90*nrow(datosX)))
# Datos de entrenamiento:
train_datosX = as.matrix(datosX[index_train,]) # datos de entrenamiento X
train_cancer = cancer[index_train] # datos de entrenamiento cancer (variable Y)
# Datos de test:
test_datosX = as.matrix(datosX[-index_train,]) # datos de test X
test_cancer =cancer[-index_train] # datos de test cancer (variable Y)


## Estandarizamos las variables X de datos train
max_datos = apply(train_datosX, 2, max)
min_datos = apply(train_datosX, 2, min)
#guardamos los datos sin estandarizar
train_datosX_sinstd=train_datosX
# estandarizamos los datos
train_datosX=as.data.frame(scale(train_datosX,center = min_datos,scale = max_datos - min_datos))


## Estandarizamos las variables X de datos test
#guardamos los datos sin estandarizar
test_datosX_sinstd=test_datosX
# estandarizamos los datos
test_datosX=as.data.frame(scale(test_datosX,center = min_datos,scale = max_datos - min_datos))



###############################################################
######   APLICAMOS UN MODELO DE XGBOOST: CAJA NEGRA      ######
###############################################################

## Modelo xgboost
set.seed(1234)
model_xgboost = xgboost::xgboost(
  data = as.matrix(train_datosX), label = train_cancer,
  nrounds = 10, objective = "binary:logistic")
summary(model_xgboost)


## Obtenemos predicciones de la red neuronal sobre los datos tests
# Aqui obtenemos las probabilidades
preds=predict(model_xgboost, as.matrix(test_datosX))
# aqui lo pasamos a clase {0,1} con el umbral 0.5
preds_class = as.numeric(preds > 0.50)              


## Tabla de contingencia de predicciones vs valores reales
preds_table = table(test_cancer, preds_class)
colnames(preds_table)= c("benign", "malignant")
row.names(preds_table)= c("benign", "malignant")
preds_table


## Valores performance del modelo
confusionMatrix(preds_table, mode = "prec_recall", positive ="malignant")



#######################################################################
######   EXPLICABILIDAD DEL MODELO MEDIANTE COUNTERFACTUALS      ######
#######################################################################

## contrafacticos sobre los datos test
summary(test_datosX_sinstd)

## seleccionamos una fila de los datos test
ix=43
data_test_ix= as.data.frame(test_datosX_sinstd) %>%  slice(ix) 

## obtenemos su prediccion
data_test_ix_std=scale(data_test_ix,center = min_datos,scale = max_datos - min_datos)
pred_ix=predict(model_xgboost, data_test_ix_std)
pred_ix


## creamos contrafactico
contrafactico=data_test_ix
# cambiamos algunos valores
contrafactico$Cl.thickness=6
contrafactico$Cell.size=6
contrafactico$Cell.shape=6
## obtenemos su prediccion
contrafactico_std=scale(contrafactico,center = min_datos,scale = max_datos - min_datos)
pred_contrafactico=predict(model_xgboost, contrafactico_std)
pred_contrafactico



## creamos contrafactico
contrafactico=data_test_ix
# cambiamos algunos valores
contrafactico$Cl.thickness=4
contrafactico$Cell.size=4
contrafactico$Cell.shape=4
## obtenemos su prediccion
contrafactico_std=scale(contrafactico,center = min_datos,scale = max_datos - min_datos)
pred_contrafactico=predict(model_xgboost, contrafactico_std)
pred_contrafactico
