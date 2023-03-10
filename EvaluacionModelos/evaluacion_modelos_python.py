##################
#### PAQUETES ####
##################

import os
import pandas as pd
import numpy as np

from sklearn.model_selection import train_test_split, KFold, cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from imblearn.metrics import classification_report_imbalanced, specificity_score
import matplotlib.pyplot as plt



###############
#### DATOS ####
###############

## leemos los datos
data= pd.read_csv(os.getcwd() + "/data/Pima.csv")
data.head()
data.shape

## refactorizamos variable target
Y = data.iloc[:, -1].to_numpy()
labels = np.unique(Y )
for i in range(len(labels)):
    Y[Y == labels[i]] = i
Y=Y.astype(int)
data.iloc[:, -1]=Y
data.head()

## dimensiones de los datos
n=data.shape[0]
p=data.shape[1]


############################
######    MUESTREO    ######
############################

n_train = 0.75
n_validation = 0.15
n_test = 1 - (n_train + n_validation)

# obtenemos data train
data_train, data_test= train_test_split(data, test_size=1 - n_train)

# obtenemos data test y data validation
data_val, data_test = train_test_split(data_test,  test_size=n_test/(n_test + n_validation))


###############################################
######    MODELO: REGRESION LOGISTICA    ######
###############################################

## datos X y datos Y en train
X_train=data_train.iloc[:,1:-1].to_numpy()
Y_train=data_train.iloc[:,-1].to_numpy()

## entreno modelo en train
logreg_model = LogisticRegression(solver='liblinear', random_state=0)
logreg_model = logreg_model.fit(X_train, Y_train)
logreg_model.coef_


#####################################
######    EVALUACION MODELO    ######
#####################################

## evaluamos modelo en validation

## datos X y datos Y en train
X_val=data_val.iloc[:,1:-1].to_numpy()
Y_val=data_val.iloc[:,-1].to_numpy()

## prediccion de probabilidad en datos validacion
Y_pred_val = logreg_model.predict_proba(X_val)
Y_pred_val=Y_pred_val[:,1]


## https://scikit-learn.org/stable/modules/classes.html#module-sklearn.metrics

## curva ROC
fpr, tpr,  _ = metrics.roc_curve(Y_val,  Y_pred_val)
plt.plot(fpr,tpr)
plt.ylabel('True Positive Rate')
plt.xlabel('False Positive Rate')
plt.axline( (0,0),slope=1,linestyle='--',color='red')
plt.show()

## area bajo la curva
auc=metrics.roc_auc_score(Y_val, Y_pred_val)
auc


### umbral de clasificacion 0.5
umbral_pred=0.5
## pasamos las predicciones de probabilidades a la clase
Y_pred_val_class=(Y_pred_val>umbral_pred)*1

## matriz de confusion
matriz_confusion=metrics.confusion_matrix(Y_val, Y_pred_val_class)
matriz_confusion

## medidas de rendimiento
medidas_rendimiento=metrics.classification_report(Y_val, Y_pred_val_class, target_names = ['class 0', 'class 1'])
print(medidas_rendimiento)



### umbral de clasificacion optimo segun curva ROC
fpr, tpr, umbrales = metrics.roc_curve(Y_val,  Y_pred_val)
opt_umbral_ROC=umbrales[np.argmax(tpr-fpr)]

## pasamos las predicciones de probabilidades a la clase
Y_pred_val_class=(Y_pred_val>opt_umbral_ROC)*1

## matriz de confusion
matriz_confusion=metrics.confusion_matrix(Y_val, Y_pred_val_class)
matriz_confusion

## medidas de rendimiento
medidas_rendimiento=metrics.classification_report(Y_val, Y_pred_val_class, target_names = ['class 0', 'class 1'])
print(medidas_rendimiento)



### umbral de clasificacion optimo segun F1
## encuentro umbral optimo segun F1
umbrales=np.arange(min(Y_pred_val), max(Y_pred_val), 0.01)
F1_scores=[]
for umbral in umbrales:
    ## pasamos las predicciones de probabilidades a la clase
    Y_pred_val_class = (Y_pred_val > umbral) * 1
    f1_umbral=metrics.f1_score(Y_val, Y_pred_val_class)
    F1_scores.append(f1_umbral)

opt_umbral_F1=umbrales[F1_scores.index(max(F1_scores))]

## pasamos las predicciones de probabilidades a la clase
Y_pred_val_class=(Y_pred_val>opt_umbral_F1)*1

## matriz de confusion
matriz_confusion=metrics.confusion_matrix(Y_val, Y_pred_val_class)
matriz_confusion

## medidas de rendimiento
medidas_rendimiento=metrics.classification_report(Y_val, Y_pred_val_class, target_names = ['class 0', 'class 1'])
print(medidas_rendimiento)



### Calculamos medida UPM (Unified Performance Measure) para datos desbalanceados

## medidas de rendimiento para imbalanced data
medidas_rendimiento_imb=classification_report_imbalanced(Y_val, Y_pred_val_class, target_names = ['class 0', 'class 1'])
print(medidas_rendimiento_imb)

## definimos funcion para upm
def upm_score(Y_true, Y_pred):
    PPV=metrics.precision_score(Y_true, Y_pred)
    TPR=metrics.recall_score(Y_true, Y_pred)
    TNR=specificity_score(Y_true, Y_pred)
    confusion_matrix=metrics.confusion_matrix(Y_true, Y_pred)
    NPV=confusion_matrix[0,0]/(confusion_matrix[0,0]+confusion_matrix[1,0])
    upm = (PPV * TPR * TNR * NPV) / (PPV * TPR * NPV + PPV * TPR * TNR + NPV * TNR * PPV + NPV * TNR * TPR)
    return upm

## calculamos upm con los datos
upm=upm_score(Y_val, Y_pred_val_class)


####################################
######    K-FOLD VALIDATION   ######
####################################

## para cross validation dividimos en train, test
n_train = 0.75
n_test = 1 - n_train

# obtenemos data train
data_train, data_test= train_test_split(data, test_size=1 - n_train)

## datos X y datos Y en train
X_train=data_train.iloc[:,1:-1].to_numpy()
Y_train=data_train.iloc[:,-1].to_numpy()

## preparamos el metodo de cross validation
cv = KFold(n_splits=10, random_state=1, shuffle=True)

## definimos el modelo que vamos a usar
model = LogisticRegression(solver='liblinear', random_state=0)

## evaluamos el modelo
scores = cross_val_score(model, X_train, Y_train, scoring='accuracy', cv=cv, n_jobs=-1)

## medidas de rendimiento (accuracy) con las que comparar modelos
print('Cross Validation accuracy scores: %s' % scores)
print('Cross Validation accuracy: %.3f +/- %.3f' % (np.mean(scores), np.std(scores)))

## si nos gusta el modelo, lo entrenamos con todos los datos
model_fit = model.fit(X_train, Y_train)

## ya podemos predecir en test
# datos X y datos Y en train
X_test=data_test.iloc[:,1:-1].to_numpy()
Y_test=data_test.iloc[:,-1].to_numpy()
# predecimos
Y_pred_test = model_fit.predict_proba(X_test)
Y_pred_test=Y_pred_test[:,1]

