#############################
#############################
######  VALORES SHAP   ######
#############################
#############################


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



####################################################################
######   EXPLICABILIDAD DEL MODELO MEDIANTE VALORES SHAP      ######
####################################################################

## Grafico para los SHAP values de todos los datos 
 # Obtenemos los valores SHAP
shap_values = shap.values(xgb_model = model_xgboost, X_train = as.matrix(train_datosX)) # podemos obtenerlo en train
shap_values = shap.values(xgb_model = model_xgboost, X_train = as.matrix(test_datosX)) # o en test

shap_values_datos = shap_values$shap_score
 # Los ponemos en el formato correcto
shap_long_datos = shap.prep(xgb_model = model_xgboost, X_train = as.matrix(train_datosX))
 # Grafico
shap.plot.summary(shap_long_datos, scientific = F)


## Grafico de importancia de las variables mediante SHAP
 # Obtenemos valores medios SHAP por variable
shap_values_variable=shap_values$mean_shap_score
 # Lo dibujamos con ploylt
m = list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
nombre_variables = names(shap_values_variable)
data_plot = data.frame(nombre_variables, shap_values_variable, stringsAsFactors = FALSE)
data_plot$nombre_variables = factor(data_plot$nombre_variables, levels = unique(data_plot$nombre_variables)[order(data_plot$shap_values_variable, decreasing = FALSE)])

fig_importancia_vbles = plot_ly(data_plot, x = ~shap_values_variable, y = ~nombre_variables,
               type = 'bar',
               marker = list(color = 'rgb(110, 36, 157)'), orientation = 'h')
fig_importancia_vbles = fig_importancia_vbles %>% layout(title = "Importancia de las variables",
                      xaxis = list(title = "Media de los valores Shap"),
                      yaxis = list(title = " "),
                      autosize = F, width = 650, height = 550, margin = m)
fig_importancia_vbles

