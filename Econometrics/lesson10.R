setwd("D:/OneDrive/���������������� �������������")
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
#�����d���� ������ ��������� survived* ��������� �������� ������� ��
Tlogic <- glm(data=T, survived~pclass+sex+age+fare+sibsp+parch, family=binomial(link="logit"), x=TRUE)
summary(Tlogic)
# ������������� ������������� ��������� ������
# ��������� ���������� �� ������� ���������� ������
# estimate ������ �������������, ����������� ��
# std.error ����������� ������ ���� ������
# z-value ��������� ������� ������ ������������ �� ����������� ������
# ������������� ������������� ������