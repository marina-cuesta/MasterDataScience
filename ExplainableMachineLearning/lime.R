#####################
#####################
######  LIME   ######
#####################
#####################


####################################
######  INSTALAMOS PAQUETES   ######
####################################

## lista de paquetes instalados
installed_packages=installed.packages()

## Lista de paquetes a usar
list.of.packages = c("mlbench",# para los datos
                     "dplyr", # para tratamiento de datos
                     "lime",# para el modelo LIME
                     "caret", # Para entrenar modelo de red neuroanl
                     "NeuralNetTools") #Para visualizar modelo de red neuronal

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
######   PREPARACION DE DATOS PARA MODELO RED NEURONAL     ######
############################################################

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
set.seed(1234) 
# indices de los datos que seran de entrenamiento
index_train = sample(1:nrow(datos_modelo),round(0.90*nrow(datos_modelo)))
# Datos de entrenamiento y test:
train_data = datos_modelo[index_train,] # datos de entrenamiento
test_data = datos_modelo[-index_train,] # datos de test


## Modelo red neuronal
# formula para el modelo
nombres_variables = names(datosX)
formula = as.formula(paste("cancer ~", paste(nombres_variables, collapse = " + ")))
# fijamos la semilla de la aleatoriedad del modelo
set.seed(1234) 
# Entrenamos red neuronal
model_nnet=caret::train(formula,data=train_data,hidden=5,  method = "nnet",linear.output=FALSE)
# Visualizamos red neuronal
model_nnet
plotnet(model_nnet$finalModel)


## Obtenemos predicciones de la red neuronal sobre los datos tests
preds = predict(model_nnet, newdata = test_data)
preds


## Tabla de contingencia de predicciones vs valores reales
preds_table = table(cancer[-index_train], preds)
colnames(preds_table)= c("benign", "malignant")
row.names(preds_table)= c("benign", "malignant")
preds_table


## Valores performance del modelo
confusionMatrix(preds_table, mode = "prec_recall")



#############################################################################
######   EXPLICABILIDAD DEL MODELO MEDIANTE MODELO SUSTITUTO LIME      ######
#############################################################################

## Usamos la funcion lime() para entrenar los modelos locales interpretables en nuestros datos train
# fijamos la semilla de la aleatoriedad del modelo
set.seed(1234) 
explainer = lime(train_data, model_nnet, bin_continuous = TRUE, quantile_bins = FALSE)


## Con la funcion explain() del paquete lime podemos obtener las explicaciones de las predicciones
## del conjunto test. Podemos pasarle la funcion a:
# todo los data test
# un subconjunto de los datos test
# un solo dato test

# Selecionamos los datos a explicar.
ix_explain=c(644,381,82,
             247,422,654)
# ix_explain=47
test_data_explain=test_data %>% 
  slice(match(ix_explain, as.numeric(row.names(test_data))))


# Obtenemos las explicaciones de esos datos  
explanation = lime::explain(test_data_explain, explainer, n_labels = 1, n_features = 6)
# Otros parametros de la funcion explain
# ??lime::explain

# Mostramos solo una parte del output para verlo mejor
explanation[, 2:10]
# View(explanation)

## Dibujamos las explicaciones
plot_features(explanation, ncol = 3)


## Mapa de calor para buscar variables comunes que influencian a todas/casi todas las obseraciones
explanation_all = lime::explain(test_data, explainer, n_labels = 1, n_features = 6)
plot_explanations(explanation_all)


