getwd()
setwd("C:/Users/Svetlana/OneDrive/���������������� �������������")
library(lmtest)
library(tseries)
library(gap)

file.show("inf1994-1999.txt")
file.show("inf2000-2016.txt")
data1 <-read.table("inf1994-1999.txt", sep="",dec=",", header = TRUE)
data2 <-read.table("inf2000-2016.txt", sep="",dec=",", header = TRUE)
y1 <- data1$inf; y2 <- data2$inf
x1 <- 1:18
dim(x1) <- c(6,3)
x1
x2 <- 1:51
dim(x2) <- c(17,3)
x1[,1]<-data1$m; x1[,2] <- data1$mr; x1[,3] <- data1$y
x2[,1]<-data2$m; x2[,2] <- data2$mr; x2[,3] <- data2$y
#���� ���
chow.test(y1,x1,y2,x2)
#���� ������-�����
modinf1 <- lm(data = data2,inf~m+mr+y)
res <- residuals(modinf1)
jarque.bera.test(res)
#���� �����
reset(modinf1)
#���� ������
bgtest(modinf1) 
#���� ������-������
bptest(modinf1)
#���� ����������� � ���������� 2 �����
#1 ���
x0 <- data.frame(m=9.9,	mr=2.64, y=1.5)
predict.lm(modinf1, newdata=x0, interval ="prediction", level=0.95)
#���� ������������ ����������� ����������
#t-value � lm
summary(modinf1)
dptest