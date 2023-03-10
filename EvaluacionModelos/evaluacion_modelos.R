##############################################
######   MACHINE LEARNING EVALUATION    ######
##############################################

library(dplyr)
# para los datos
library(pdp)
#para decision tree
library(rpart)
# para dibujar decision tree
library(rpart.plot)

# para matriz de confusion, medidas de rendimiento y k-fold validation
library(caret)
# para curva ROC
library(ROCR)
library(pROC)

########################
######   DATOS    ######
########################

# DATOS PIMA: https://rdrr.io/cran/pdp/man/pima.html

## Leemos los datos
data(pima)
data = pima
head(data)

## tipo de variables
str(data)

## re factorizamos variable target
# 0 --> neg
# 1 --> pos
data$diabetes=factor(data$diabetes, labels = c(0, 1),levels=c("neg","pos"))


## resumen de los datos
summary(data)

## quito las variables insulin y triceps
data=data %>% select(-c(insulin,triceps))

## quito valores NA
data=na.omit(data)

## dimensiones de los datos
dim(data)
n=dim(data)[1]
p=dim(data)[2]

## resumen de los datos
summary(data)

## distribucion variable respuesta
table(data$diabetes)/n*100



############################
######    MUESTREO    ######
############################

# Porcentajes de train, test y validation
n_train = .75
n_validation = .15
n_test = 1 - (n_train + n_validation)

# Muestreo
set.seed(123)
indices=1:n
indices_train = sample(1:n, n * n_train)
indices_test = sample(indices[-indices_train], n * n_test)
indices_validation = indices[-c(indices_train,indices_test)]

# separamos la base de datos en train, validation, test
data_train = data[indices_train,]
data_test = data[indices_test,]
data_validation = data[indices_validation,]



#################################################
######    MODELO 1: REGRESION LOGISTICA    ######
#################################################

## ajuste del modelo
logreg_model <- glm(diabetes ~ . - pedigree,
                data = data_train,  family=binomial(link = "logit"))
summary(logreg_model)


###########################################
######    MODELO 2: DECISION TREE    ######
###########################################


## arbol eligiendo cp. empiezo con sobre ajuste y luego podo (prune)
## ajustamos modelo
set.seed(12) # hay que fijar semilla porque rpart usa random sampling
decisiontree_max=rpart(diabetes~., data=data_train,
             method='class',parms=list(split="gini"),control=c(minsplit=0),cp=0)

cp_values=decisiontree_max$cptable
cp_select=cp_values[cp_values[,"xerror"]==min(cp_values[,"xerror"]),"CP"]
cp_select

# ahora lo podamos
decisiontree_model=prune.rpart(decisiontree_max, cp=cp_select)
summary(decisiontree_model)
rpart.plot(decisiontree_model)



#############################################################
######    EVALUACION MODELO 1 EN DATOS DE VALIDACION   ######
#############################################################

## predicciones del modelo en la muestra de validacion. Devuelve probabilidad
logreg_pred <- predict(logreg_model, newdata = data_validation, type = "response")

## Curva ROC
logreg_prediction = prediction(logreg_pred,data_validation$diabetes)
logreg_performance = performance(logreg_prediction,measure = "tpr",x.measure = "fpr") # fpr= 1- spec
plot(logreg_performance)
abline(0,1)

## Area bajo la curva
logreg_auc=auc(data_validation$diabetes,logreg_pred)
logreg_auc


### pasamos las predicciones de probabilidades a la clase con umbral 0.5
umbral_pred=0.5
logreg_pred_class=as.numeric(logreg_pred>umbral_pred)

# matriz de confusion 
obs=data_validation$diabetes
logreg_table=table(logreg_pred_class,obs)

# medidas de rendimiento
# https://rdrr.io/cran/caret/man/confusionMatrix.html
confusionMatrix(logreg_table, positive="1")
confusionMatrix(logreg_table, mode="prec_recall", positive="1")




### elegimos el mejor umbral en funcion de la curva roc
roc_curve_values <- data.frame(cut=logreg_performance@alpha.values[[1]], fpr=logreg_performance@x.values[[1]], 
                               tpr=logreg_performance@y.values[[1]])
roc_curve_values$diff=roc_curve_values$tpr-roc_curve_values$fpr
opt_threshold_roc=roc_curve_values$cut[which.max(roc_curve_values$diff)]

## pasamos las predicciones de probabilidades a la clase con umbral optimo segun curva ROC
logreg_pred_class=as.numeric(logreg_pred>opt_threshold_roc)

# matriz de confusion 
obs=data_validation$diabetes
logreg_table=table(logreg_pred_class,obs)

# medidas de rendimiento
confusionMatrix(logreg_table, positive="1")
confusionMatrix(logreg_table, mode="prec_recall", positive="1")



### elegimos umbral maximizando F1
F1_scores=data.frame(umbral=seq(min(logreg_pred),max(logreg_pred),0.01),
                     F1=numeric(length(seq(min(logreg_pred),max(logreg_pred),0.01))))
for (umbral in F1_scores$umbral){
  logreg_pred_class=as.numeric(logreg_pred>umbral)
  tabla_pred=table(logreg_pred_class,obs)
  medidas=confusionMatrix(tabla_pred, mode = "prec_recall", positive="1")
  F1=medidas$byClass["F1"]
  F1_scores$F1[F1_scores$umbral==umbral]=F1
}
opt_threshold_F1=F1_scores$umbral[which.max(F1_scores$F1)]

## pasamos las predicciones de probabilidades a la clase con umbral optimo segun F1
logreg_pred_class=as.numeric(logreg_pred>opt_threshold_F1)

# matriz de confusion 
obs=data_validation$diabetes
logreg_table=table(logreg_pred_class,obs)

# medidas de rendimiento
confusionMatrix(logreg_table, positive="1")
confusionMatrix(logreg_table, mode="prec_recall", positive="1")



## Calculamos UPM (Unified Performance Measure) para datos desbalanceados
upm_score <- function(matriz_confusion){
  ## calculamos medidas de rendimiento
  medidas_prec_reall=confusionMatrix(matriz_confusion, mode = "prec_recall", positive="1")
  medidas_specificity=confusionMatrix(matriz_confusion, positive="1")
  ## extraemos las medidas que nos interesan
  PPV=medidas_prec_reall$byClass["Precision"]
  TPR=medidas_prec_reall$byClass["Recall"]
  TNR=medidas_specificity$byClass["Specificity"]
  NPV=medidas_specificity$byClass["Neg Pred Value"]
  
  ## calculamos UPM
  upm=(PPV*TPR*TNR*NPV)/(PPV*TPR*NPV + PPV*TPR*TNR + NPV*TNR*PPV + NPV*TNR*TPR)
  names(upm)=NULL
  return(upm)
}

logreg_upm=upm_score(logreg_table)



#############################################################
######    EVALUACION MODELO 2 EN DATOS DE VALIDACION   ######
#############################################################

## predicciones del modelo en la muestra de validacion. Devuelve probabilidad
decisiontree_pred <- predict(decisiontree_model, newdata = data_validation, type = "prob")
decisiontree_pred=decisiontree_pred[,2]

## Curva ROC
decisiontree_prediction = prediction(decisiontree_pred,data_validation$diabetes)
decisiontree_performance = performance(decisiontree_prediction,measure = "tpr",x.measure = "fpr") # fpr= 1- spec
plot(decisiontree_performance)
abline(0,1)

## Area bajo la curva
decisiontree_auc=auc(data_validation$diabetes,decisiontree_pred)
decisiontree_auc


### pasamos las predicciones de probabilidades a la clase con umbral 0.5
umbral_pred=0.5
decisiontree_pred_class=as.numeric(decisiontree_pred>umbral_pred)

# matriz de confusion 
obs=data_validation$diabetes
decisiontree_table=table(decisiontree_pred_class,obs)

# medidas de rendimiento
# https://rdrr.io/cran/caret/man/confusionMatrix.html
confusionMatrix(decisiontree_table, positive="1")
confusionMatrix(decisiontree_table, mode="prec_recall", positive="1")




### elegimos el mejor umbral en funcion de la curva roc
roc_curve_values <- data.frame(cut=decisiontree_performance@alpha.values[[1]], fpr=decisiontree_performance@x.values[[1]], 
                               tpr=decisiontree_performance@y.values[[1]])
roc_curve_values$diff=roc_curve_values$tpr-roc_curve_values$fpr
opt_threshold_roc=roc_curve_values$cut[which.max(roc_curve_values$diff)]

## pasamos las predicciones de probabilidades a la clase con umbral optimo segun curva ROC
decisiontree_pred_class=as.numeric(decisiontree_pred>opt_threshold_roc)

# matriz de confusion 
obs=data_validation$diabetes
decisiontree_table=table(decisiontree_pred_class,obs)

# medidas de rendimiento
confusionMatrix(decisiontree_table, positive="1")
confusionMatrix(decisiontree_table, mode="prec_recall", positive="1")



### elegimos umbral maximizando F1
F1_scores=data.frame(umbral=seq(min(decisiontree_pred),max(decisiontree_pred),0.01),
                     F1=numeric(length(seq(min(decisiontree_pred),max(decisiontree_pred),0.01))))
for (umbral in F1_scores$umbral){
  decisiontree_pred_class=as.numeric(decisiontree_pred>umbral)
  tabla_pred=table(decisiontree_pred_class,obs)
  medidas=confusionMatrix(tabla_pred, mode = "prec_recall", positive="1")
  F1=medidas$byClass["F1"]
  F1_scores$F1[F1_scores$umbral==umbral]=F1
}
opt_threshold_F1=F1_scores$umbral[which.max(F1_scores$F1)]

## pasamos las predicciones de probabilidades a la clase con umbral optimo segun F1
decisiontree_pred_class=as.numeric(decisiontree_pred>opt_threshold_F1)

# matriz de confusion 
obs=data_validation$diabetes
decisiontree_table=table(decisiontree_pred_class,obs)

# medidas de rendimiento
confusionMatrix(decisiontree_table, positive="1")
confusionMatrix(decisiontree_table, mode="prec_recall", positive="1")



## Calculamos UPM (Unified Performance Measure) para datos desbalanceados
decisiontree_upm=upm_score(decisiontree_table)




############################################
######    SELECCION DE MEJOR MODELO   ######
############################################

## fijandome en F1 decido decision tree con umbral opt_threshold_F1

## obtengo predicciones del modelo en la muestra de test 
decisiontree_pred_test <- predict(decisiontree_model, newdata = data_test, type = "prob")
decisiontree_pred_test=decisiontree_pred_test[,2]


## pasamos las predicciones de probabilidades a la clase con umbral optimo segun F1
decisiontree_pred_test_class=as.numeric(decisiontree_pred_test>opt_threshold_F1)

# matriz de confusion 
obs=data_test$diabetes
decisiontree_table_test=table(decisiontree_pred_test_class,obs)

# medidas de rendimiento
confusionMatrix(decisiontree_table_test, positive="1")
confusionMatrix(decisiontree_table_test, mode="prec_recall", positive="1")

## ¿seran mejores que las de regresion logistica? En cualquier caso, ya habriamos decidido el modelo
# y no podemos cambiar de decision






####################################
######    K-FOLD validation   ######
####################################

## paquete caret
# https://topepo.github.io/caret/available-models.html


## para cross validation dividimos en train, test

# Porcentajes de train, test y validation
n_train = .75
n_test = 1 - n_train

# Muestreo
set.seed(123)
indices=1:n
indices_train = sample(1:n, n * n_train)
indices_test = indices[-c(indices_train)]

# separamos la base de datos en train, validation, test
data_train = data[indices_train,]
data_test = data[indices_test,]


## fijamos la semilla
set.seed(123)

## definimos la muestra k fold
train_control <- trainControl(method = "cv",  number = 10)


## entrenamos decision tree con funcion train
model <- train(diabetes~., data = data_train,
               trControl = train_control,
               method = "rpart")
model

## ya podemos predecir en test
pred_test=predict(model,type="prob")

