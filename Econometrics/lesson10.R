setwd("D:/OneDrive/Эконометрическое моделирование")
library(dplyr)
T <- read.table("titanic.txt", header=TRUE, sep=",",dec=".")
glimpse(T)
library(erer)
library(glmnet)

library(vcd)
library(AUC)
summary(T)
T <- mutate(T, survived=as.factor(survived));glimpse(T)
summary(T)
#Оцениdание модели выживания survived* пассажира титаника методом МП
Tlogic <- glm(data=T, survived~pclass+sex+age+fare+sibsp+parch, family=binomial(link="logit"), x=TRUE)
summary(Tlogic)
# интерпретация коэффициентов оцененной модели
# структура информации об оценках параметров модели
# estimate оценки коэффициентов, вычисленных МП
# std.error стандартные ошибки этих оценок
# z-value результат деления оценки коэффициента на стандартную ошибку
# интерпертация коэффициентов модели