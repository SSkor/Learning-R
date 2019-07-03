setwd("D:/OneDrive/Эконометрическое моделирование")
library(dplyr)
library(erer)
library(glmnet)
library(vcd)
library(AUC)
T<-read.table('titanic.txt',sep=',', dec='.', header=TRUE)
T<-mutate(T,survived=as.factor(survived))
glimpse(T)
##30/11/2018----
library(ggplot2)
#проверка гипотез о параметрах glm модели на примере гипотезы 
#H0:коэффициент a6=0 (проверка незначимости цены(fare))
#H1:коэффициент a6!=0
#заполняем массив T1
T1 <- data.frame(T$survived, T$pclass,T$sex, T$age, T$fare, T$sibsp, T$parch)
glimpse(T1)
#очистим массив Т1 от пропущенных данных при помощи комманды na.omit
T2 <- na.omit(T1)
glimpse(T2)
#оценка модели
# Tlogic<-glm(data=T2,T.survived~T.pclass+T.sex+T.age+T.fare+T.sibsp+T.parch, 
#             family=binomial(link='logit'),
#             x=TRUE)
# summary(Tlogic)
Tlogic<-glm(data=T2,T.survived~T.pclass+T.sex+T.age+T.fare+T.sibsp+T.parch, 
            family=binomial(link='logit'),
            x=TRUE)
summary(Tlogic)
#оценка модели без fare
# Tlogic0<-glm(data=T2,T.survived~T.pclass+T.sex+T.age+T.fare+T.sibsp, 
#             family=binomial(link='logit'),
#             x=TRUE)
# summary(Tlogic0)
#выполняем LR-тест гипотезы H0
# lrtest(Tlogic0, Tlogic)

##Построение ROC-кривой----
#Прогнозз латентной переменной
T_pr<-predict.glm(Tlogic,T2,se=TRUE)
T2 <- cbind(T2, T_pr)
T2 <- mutate(T2, prob = plogis(fit))
glimpse(T2)
#roc.data.new <- roc(T2$prob,T2$T.survived)
roc.data.new <- roc(T2$prob,T2$T.survived)

qplot(x=roc.data.new$fpr,y=roc.data.new$tpr, geom="line")
test_data <- data.frame(x=roc.data.new$fpr, 
                        y1=roc.data.new$tpr, 
                        y2=roc.data.old$tpr)
ggplot(test_data, aes(x)) + 
  geom_line(aes(y = y1, colour = 'red')) + 
  geom_line(aes(y = y2, colour = "black"))