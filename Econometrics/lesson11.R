setwd("D:/OneDrive/Ёконометрическое моделирование")
library(dplyr)
T <- read.table("titanic.txt", header=TRUE, sep=",",dec=".")
glimpse(T)
View(T)
library(erer)
library(glmnet)

library(vcd)
library(AUC)
summary(T)
T <- mutate(T, survived=as.factor(survived));glimpse(T)
summary(T)
#ќцениdание модели выживани€ survived* пассажира титаника методом ћѕ
Tlogic <- glm(data=T, survived~pclass+sex+age+fare+sibsp+parch, family=binomial(link="logit"), x=TRUE)
summary(Tlogic)
newdata <- data.frame(age=c(26,56,57), 
                      sex=c("female","female","male"), 
                      pclass=c("2nd", "2nd", "2nd"), 
                      fare=c(29, 21, 20),
                      sibsp=c(0,1,6),
                      parch=c(2,1,1))
#прогнозируем значени€ survived*
pr_Tlogic <- predict.glm(Tlogic, newdata = newdata, se=TRUE)
newdata_pr <- cbind(newdata, pr_Tlogic)
#прогнозируем то, что survived=1
newdata_pr <- mutate(newdata_pr, prob=plogis(fit)) 
#интервальное прогнозирование значений P(survived=1)
newdata_pr <- mutate(newdata_pr, prob=plogis(fit),
                     L=plogis(fit-1.96*se.fit), R=plogis(fit+1.96*se.fit))
#рассчет предельных эффектов
maBina(Tlogic)
