#https://rpubs.com/raviolli77/352956
#https://rpubs.com/H_Zhu/235617
#https://rpubs.com/elfenixsoy/arbol-veronica

#Carga de archivo csv
credit <- read.csv('/home/newuser/Descargas/credit.csv')

#Limpieza de datos valores unknown variables checking_balance & savings_balance
library(dplyr)
clearCredit <- credit %>%
  select(everything()) %>%
  filter(credit$checking_balance != "unknown" & credit$savings_balance != "unknown")

#Validar tipo de datos
str(clearCredit)
library(ggplot2)
ggplot(data=clearCredit,aes(x=default)) + geom_bar() + geom_text(stat='Count',aes(label=..count..),vjust=-1);
#======================= VARIABLES CATEGORICAS =============================
table(clearCredit$checking_balance) #
table(clearCredit$credit_history) #Historico crediticio
table(clearCredit$purpose) #Proposito
table(clearCredit$savings_balance) #Saldo de Ahorros
table(clearCredit$employment_duration) #Duracion de empleo
table(clearCredit$other_credit) #Otros creditos
table(clearCredit$housing) #Vivienda
table(clearCredit$job) #Trabajo
table(clearCredit$phone) #Tinen o numero de telefono
table(clearCredit$default) #Indica si fue aprobado o no el credito
#===========================VARIABLES NUMERICAS===============
clearCredit$months_loan_duration #mese de duracion de prestamo
clearCredit$amount # suma
clearCredit$percent_of_income #porcentaje de ingresos
clearCredit$years_at_residence #años de recidencia
clearCredit$age #edad
clearCredit$existing_loans_count #numero de prestamos existentes
clearCredit$dependents #dependientes


barplot(table(clearCredit$default),
        main = "Relación de creditos solicitados",
        xlab = "Desicion", ylab = "Frecuencia",
        col = c("royalblue","seagreen")
)
legend("topright",
       c("no","yes"),
       fill = c("royalblue","seagreen")
)

numericVar <- clearCredit[,-c(0:1)]
numericVar <- numericVar[,-8]
library(corrplot)
corMatMy <- cor(numericVar[,1:7])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)
#===================================CORRELACION ENTRE VARIABLES ====================
#Grafico relacion default vs age
ggplot(clearCredit) + aes(x=as.numeric(age), 
                          group=default, fill=default) + 
  geom_histogram(binwidth=1, color='black') +
  ggtitle('Relación default vs age')

library('ggplot2')
posn.j <- position_jitter(0.7,0)
ggplot(clearCredit,aes(x=factor(checking_balance),y=amount,col=factor(checking_balance)))+
  theme(axis.title.y = element_text(margin = margin(t = 30, r = 30, b = 0, l = 30)))+
  geom_jitter(size=3,alpha=0.5,position = posn.j)+
  facet_grid(". ~ default")

ggplot(clearCredit,aes(x=factor(checking_balance),fill=factor(checking_balance)))+
  geom_bar(position = "dodge")+
  facet_grid(". ~ default")


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

data_train <- create_train_test(clearCredit, 0.8, train = TRUE)
data_test <- create_train_test(clearCredit, 0.8, train = FALSE)

library('rpart')
#Entrenamiento
train_tree <- rpart(default ~ checking_balance  +
                     + employment_duration + age +
                      housing + job + credit_history+
                      amount  +percent_of_income + 
                      existing_loans_count + dependents, data = data_train, method = "class" )
library('rattle')
library('rpart.plot')
library('RColorBrewer')
#graficar el arbol
fancyRpartPlot(train_tree)

# Hacer prediccion basado en el test de prueba
my_prediction <- predict(train_tree,data_test,type="class")

library('rpart')
my_tree_three <-  rpart(default ~ checking_balance  + age +
                          + employment_duration +
                          housing + job + credit_history+
                          amount  +percent_of_income + 
                          existing_loans_count + dependents, data = data_train, method = "class", control = rpart.control(minsplit = 50, cp =0))



library('rattle')
library('rpart.plot')
library('RColorBrewer')
fancyRpartPlot(my_tree_three)






