file.show("G.txt")
library(glmnet)
tableG<-read.table("G.txt", sep="",dec=",",header = TRUE)
tableG
Skorik<-lm(data=tableG, Gt~Gt.1+CrSt+0)
summary(Skorik)