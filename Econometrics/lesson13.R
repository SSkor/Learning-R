setwd("D:/OneDrive/���������������� �������������")
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
#���������� ������� ��� ��������� ������ ������
#���������� OR �������� ������
ORmod <- plm(y~x, data=UR, model = "pooling")
#���������� ������ � ������������� ����������� FE
FEmod <- plm(y~x, data=UR, model = "within")
#������� �� ����� �������� ������������� ��������
FE <- fixef(FEmod);FE
summary(FEmod)
#������������ �������� H0: ������ pooling ���������������� ������ within 
#�.�. �������������� ������� �����������
pooltest(y~x,data=UR, model = "within")

####�������� ������ �14====

####���������� UR ���������
RUS <- UR[1:9,3:4]
USA <- UR[10:18,3:4]
RUSmod <- lm(y~x, RUS); summary(RUSmod)
USAmod <- lm(y~x, USA); summary(USAmod)
#���������� RE ���������
#�� ��������. ������. �� ����� ���-�� ������� ��������..!
REmod <- plm(y~x, data=UR, model = "random")
#������������ �������� H0: ������ pooling ���������������� ������ within 
#�.�. �������������� ������� �����������
pooltest(y~x,data=UR, model = "random")

#####Grunfeld----
#������� ����������
ORmodGrun <- plm(inv~value+capital, data=Grunfeld, model = "pooling"); summary(ORmodGrun)
#������ � ��������������� ���������
FEmodGrun <- plm(inv~value+capital, data=Grunfeld, model = "within"); summary(FEmodGrun)
FEGun <- fixef(FEmodGrun)
#������������ �������� H0: ������ pooling ���������������� ������ within
pooltest(inv~value+capital, data=Grunfeld, model = "within")
#pooltest(inv~value+capital, data=Grunfeld, model = "random") 
#�� ��������� �������� ����, ��� ������� ������ ��������

