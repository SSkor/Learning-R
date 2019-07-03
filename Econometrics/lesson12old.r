getwd()
file.show('tit.txt')
data1<-read.table('tit.txt',sep=',',header=TRUE)
library(dplyr)
library(erer)
library(glmnet)
library(vcd)
library(AUC)
T<-read.table('tit.txt',sep=',', dec='.', header=TRUE)
glimpse(T)
T<-mutate(T,survived=as.factor(survived))
# glimpse(T)
# summary(T)
# #Оценивание модели survived* ММП (*-латентная переменная)
# Tlogic<-glm(data=T,survived~pclass+sex+age+fare,family=binomial(link='logit'),x=TRUE)
# summary(Tlogic)
# newdata<-data.frame(age=c(26,56,57),sex=c('female','female','male'),
#                     pclass=c('2nd','2nd','2nd'),fare=c(29,21,20))
# newdata
# #прогноз значения survived* (8.1)
# pr_Tlogic<-predict.glm(Tlogic,newdata,se=TRUE)
# newdata_pr<-cbind(newdata,pr_Tlogic);newdata_pr
# #прогнозируем значения Р(survived=1) (8.2)
# newdata_pr<-mutate(newdata_pr,prob=plogis(fit));newdata_pr
# #интервальное прогнозирование значяения Р(survived=1)(8.3-8.4)
# newdata_pr<-mutate(newdata_pr,prob=plogis(fit),
#                    L=plogis(fit-1.96*se.fit),R=plogis(fit+1.96*se.fit));newdata_pr
# ##################################
# #рассчет предельных эффектов
# maBina(Tlogic)
# 
# ##30/11/2018----
# library(ggplot2)
#проверка гипотез о параметрах glm модели на примере гипотезы 
#H0:коэффициент a6=0 (проверка незначимости цены(fare))
#H1:коэффициент a6!=0
#заполняем массив T1
T1 <- data.frame(T$survived, T$pclass,T$sex, T$age, T$fare)
glimpse(T1)
#очистим массив Т1 от пропущенных данных при помощи комманды na.omit
T2 <- na.omit(T1)
glimpse(T2)
#оценка модели
Tlogic<-glm(data=T2,T.survived~T.pclass+T.sex+T.age+T.fare, 
            family=binomial(link='logit'),
            x=TRUE)
summary(Tlogic)
#оценка модели без fare
# Tlogic0<-glm(data=T2,T.survived~T.pclass+T.sex+T.age, 
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
roc.data.old <- roc(T2$prob,T2$T.survived)
glimpse(roc.data.old)
qplot(x=roc.data.old$fpr,y=roc.data.old$tpr, geom="line")
#plot two rocs
test_data1 <- data.frame(x1=roc.data.new$fpr, 
                         y1=roc.data.new$tpr)
test_data2 <- data.frame(x2=roc.data.old$fpr,
                         y2=roc.data.old$tpr)
ggplot() + 
  geom_line(test_data1, mapping = aes(x=x1, y = y1, colour = 'new')) + 
  geom_line(test_data2, mapping = aes(x=x2, y = y2, colour = 'old'))
# test_data1 %>% filter(x1 >= 0.005) %>% select(y1) %>% length() -> fgh