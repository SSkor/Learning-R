setwd("C:/Users/Svetlana/OneDrive/Эконометрическое моделирование")
library(dplyr)
T <- read.table("titanic.txt", header=TRUE, sep=",",dec=".")
glimpse(T)
library(erer)
library(glmnet)

library(vcd)
library(OUC)
summary(T)