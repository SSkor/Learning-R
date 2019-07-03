file.show("IY.txt")
library(glmnet)
tableIY<-read.table("IY.txt", sep="",dec=",",header = TRUE)
tableIY
Skorik<-lm(data=tableIY, It~d.t.1+CrSt)
summary(Skorik)