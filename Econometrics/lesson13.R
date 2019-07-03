setwd("D:/OneDrive/Эконометрическое моделирование")
library(plm)
library(dplyr)
data(package="plm")
data(Grunfeld)
head(Grunfeld)
tail(Grunfeld)
glimpse(Grunfeld)
getwd()
file.show("ur.txt")
UR <- read.table("ur.txt", sep="", dec=",", header=TRUE)
glimpse(UR)
UR <- pdata.frame(UR, index=c("country","year"), row.names = TRUE)
glimpse(UR)
head(UR)
tail(UR)
#Оценивание моделей для панельных данных Оукена
#Оценивание OR регресии Оукена
ORmod <- plm(y~x, data=UR, model = "pooling")
#Оценивание модели с фиксироваными переменными FE
FEmod <- plm(y~x, data=UR, model = "within")
#Выведем на экран значения фиксированных эффектов
FE <- fixef(FEmod);FE
summary(FEmod)
#Протестируем гипотезу H0: модель pooling предпочтительнее модели within 
#т.е. индивидульаные эффеткы отстутсвуют
pooltest(y~x,data=UR, model = "within")

####Домашняя работа №14====

####Оценивание UR регрессии
RUS <- UR[1:9,3:4]
USA <- UR[10:18,3:4]
RUSmod <- lm(y~x, RUS); summary(RUSmod)
USAmod <- lm(y~x, USA); summary(USAmod)
#Оценивание RE регрессии
#НЕ РАБОТАЕТ. ОШИБКА. НЕ МОЖЕТ ГДЕ-ТО МАТРИЦУ ОБРАТИТЬ..!
REmod <- plm(y~x, data=UR, model = "random")
#Протестируем гипотезу H0: модель pooling предпочтительнее модели within 
#т.е. индивидульаные эффеткы отстутсвуют
pooltest(y~x,data=UR, model = "random")

#####Grunfeld----
#Обычная регерессия
ORmodGrun <- plm(inv~value+capital, data=Grunfeld, model = "pooling"); summary(ORmodGrun)
#Модель с фикисированными эффектами
FEmodGrun <- plm(inv~value+capital, data=Grunfeld, model = "within"); summary(FEmodGrun)
FEGun <- fixef(FEmodGrun)
#Тестирование гипотезы H0: модель pooling предпочтительнее модели within
pooltest(inv~value+capital, data=Grunfeld, model = "within")
#pooltest(inv~value+capital, data=Grunfeld, model = "random") 
#не запускать Проверка того, что функция вообще работает

