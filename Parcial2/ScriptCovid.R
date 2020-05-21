library("dplyr")
library(lubridate)
library('ggplot2')
library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')
library(caret)
library(ROCR)
# 1) Preprocesamiento de datos y limpieza
#Carga de Dataset
path <- '/home/newuser/Documentos/RDocuments/Parcial2/DataSetCovid.csv'  
DataCovid <- read.csv(path,check.names = F, sep = ';' )
str(DataCovid)
#Cambio nombre cabeceras
colnames(DataCovid) <- c('Fecha_diagnostico','Ciudad_residencia',
                         'Localidad_residencia','Edad','Sexo','Tipo_caso',
                         'Ubicacion','Estado')


#Limpieza de datos para Localidad_residencia
library("dplyr")
CleanDataSet <- DataCovid %>%
  select(everything()) %>%
  filter(Localidad_residencia != '22 - Sin Dato')


#Tipo_caso relacionado = Relacionado
CleanDataSet$Tipo_caso[CleanDataSet$Tipo_caso == "En estudio"] <- "En Estudio"
CleanDataSet$Tipo_caso[CleanDataSet$Tipo_caso == "relacionado"] <- "Relacionado"
##Tipo_caso relacionado = Relacionado

#Validar rango de edades min= 1 max=103
ageSort <- sort(DataCovid$Edad)
rango <- range(DataCovid$Edad)


#Sepracacion de la variable Localidad_residencia en dos
CleanDataSet <- within(data=CleanDataSet, id_residencia<-as_data_frame
                     (do.call('rbind',strsplit(as.character(Localidad_residencia),"-",fixed=TRUE))))

#Definimos vector de extremos de rango de edades 
L = c(1,10,20,30,40,50,60,70,80,90,100,110)
#Tabla de rango de edaddes
edades =  transform(table(cut(DataCovid$Edad, breaks = L, right = FALSE, include.lowest = TRUE)))   
edades
plot(edades, main = "¿Frecuencia segun rangos de edad?")

#Cambio de valores int edad por el rango correspondiente
CleanDataSet$Edad <- cut(CleanDataSet$Edad, breaks = L, right = FALSE, include.lowest = TRUE)


#Cambio fecha completa por mes
library(lubridate)
CleanDataSet$Fecha_diagnostico <- month(as.POSIXlt(CleanDataSet$Fecha_diagnostico,
                                                   format="%d/%m/%Y"))

CleanDataSet$Fecha_diagnostico[CleanDataSet$Fecha_diagnostico == 3] <- "Marzo"
CleanDataSet$Fecha_diagnostico[CleanDataSet$Fecha_diagnostico == 4] <- "Abril"
#=============================================================================================
#2. Realizar la correlación de información entre el género el resto de las variables
#Cambiar por cada variable

#Grafica de frecuencia de casos segun genero
ggplot(data=CleanDataSet,aes(x=Sexo)) 
+ geom_bar() 
+ geom_text(stat='Count',aes(label=..count..),vjust=-1)

# Graficos edad vs variable
library('ggplot2')
ggplot(CleanDataSet,aes(x=factor(Tipo_caso),fill=factor(Tipo_caso)))+
  ggtitle ("Genero vs Tipo de caso") +
  geom_bar(position = "dodge")+
  facet_grid(". ~Sexo")


#5. Graficos localidad vs ubicacion
library('ggplot2')
ggplot(CleanDataSet,aes(x=factor(Ubicacion), with=1, fill=factor(Localidad_residencia)))+
  ggtitle ("Localidad vs Ubicacion") +
  geom_bar(position = "dodge")

#6. Correlacionar el origen del caso contra la localidad, y las otras variables.

library('ggplot2')
ggplot(CleanDataSet,aes(x=factor(Tipo_caso), with=1, fill=factor(Edad)))+
  ggtitle ("Origen vs Edad") +
  geom_bar(position = "dodge")

#7 Arboles de desicion

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row <- nrow(data)
  total_row <- size * n_row
  train_sample <- 1:total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

#COnstruccion de data set Entrenamiento 80% - prueba 20%
data_train <- create_train_test(CleanDataSet, 0.8, train = TRUE)
data_test <- create_train_test(CleanDataSet, 0.8, train = FALSE)

dim(data_train)

#==========================MODELO===========================
library('rpart')
my_tree_one <- rpart(Estado ~   Sexo + Edad + id_residencia$V1, data = data_train, method = 'class')
my_tree_two <- rpart(Estado ~   Sexo + Edad + id_residencia$V1 + Tipo_caso, data = data_train, method = 'class')
my_tree_three <- rpart(Estado ~  Ubicacion + Edad + Localidad_residencia + Ciudad_residencia, data = data_train, method = 'class')
    
library('rattle')
library('rpart.plot')
library('RColorBrewer')
#graficar el arbol
fancyRpartPlot(my_tree_one)
fancyRpartPlot(my_tree_two)
fancyRpartPlot(my_tree_three)

#===========================PREDICCION============================

prediccion_1 <- predict(my_tree_one, newdata = data_test, type = "class")
prediccion_2 <- predict(my_tree_two, newdata = data_test, type = "class")
prediccion_3 <- predict(my_tree_three, newdata = data_test, type = "class")

library(ROCR)

#Matrices de confusion
library(caret)
confusionMatrix(prediccion_1, data_test[["Estado"]])
confusionMatrix(prediccion_2, data_test[["Estado"]])
confusionMatrix(prediccion_3, data_test[["Estado"]])


predict.rpart <- predict(my_tree_two,data_train,type = "prob")[,2] #prob. clase=yes
predict.rocr  <- prediction (predict.rpart,data_train$Estado)
perf.rocr     <- performance(predict.rocr,"tpr","fpr") #True y False postivie.rate

auc <- as.numeric(performance(predict.rocr ,"auc")@y.values)
plot(perf.rocr,type='o', main = paste('Area Bajo la Curva =',round(auc,2)))  
abline(a=0, b= 1)


# GRAFICO CURVA ROC
#------------------------------------------------------------------------------




library(ROCR)
pr <- prediction(as.numeric(prediccion_1, data_test$Estado), as.numeric(y))
pr <- prediction(prediccion_1, data_test$Estado)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")

# create a data frame for TP and FP rates
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])

# create a data frame for TP and FP rates
dd <- data.frame(FP = prf@x.values[[1]], TP = prf@y.values[[1]])


#================= GRAFICOS COMPLEMENTARIOS =====================
library('ggplot2')
ggplot(CleanDataSet,aes(x=factor(Sexo),fill=factor(Localidad_residencia)))+
  ggtitle ("Estado segun edad") +
  geom_bar(position = "dodge")+
  facet_grid(". ~Estado")

basicplot <- ggplot(data_test, aes(d = D, m = M1)) + plotROC::geom_roc()
basicplot

library(ggplot2)
ggplot(data=CleanDataSet,aes(x=Estado)) 
+ geom_bar() 
+ geom_text(stat='Count',aes(label=..count..),vjust=-1);



discretizacion <- CleanDataSet
str(discretizacion)
discretizacion$Ciudad_residencia <- as.numeric(discretizacion$Ciudad_residencia)
discretizacion$Estado<- as.numeric(discretizacion$Estado)
discretizacion$Ubicacion <- as.numeric(discretizacion$Ubicacion)
discretizacion$Tipo_caso <- as.numeric(discretizacion$Tipo_caso)
discretizacion$Edad <- as.numeric(discretizacion$Edad)
discretizacion$Sexo <- as.numeric(discretizacion$Sexo)
discretizacion$Localidad_residencia <- as.numeric(discretizacion$Localidad_residencia)

library(corrplot)
## corrplot 0.84 loaded
M <- cor(discretizacion)
corrplot(M, method = "circle")
corrplot(M, method = "number")





