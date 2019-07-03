#��������� ��3-3########################################################################################
set.seed(0) #��� ������������� 

MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)] #������ �����������, MTSS[-length(MTSS)] drops the last element of a

RTSI <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/RI.RTSI_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rRTSI <- diff(RTSI)/RTSI[-length(RTSI)]

#CML
rf <- 0.05 #����������� ������
erRTSI <- mean(rRTSI)
stRTSI <- sd(rRTSI)
curve( stRTSI/(erRTSI-rf)*(x-rf), from = 0, to = 1, 
       xlab = "Expected Returns", ylab = "Volatility", main = "Capital Market Line") #� ����� ����� ������

#stock's
#����������� ����������� MTSS
rMTSSmin <- min(rMTSS)
rMTSSmax <- max(rMTSS)
bins <- seq(rMTSSmin, rMTSSmax, length.out = 20) #�������
hist(rMTSS, breaks = bins, prob=TRUE) 

#��������� 29.03.18#####################################################################################
#����������� ��������������, ����� ������� �����, ������������� �� ������������. ��������� �� ����������
library(fBasics) #��������� 29.03.18
kurtosisMTSS <- basicStats(rMTSS)["Kurtosis",]
skewnessMTSS <- basicStats(rMTSS)["Skewness",]
#� ����������� ������������� �������� = 3, � �����.���������� = 0. � ������������� ����������� ��� Skewness = -0.809858
#Kurtosis = 6.034835. ������������� ���������� ��������������� � ���, ��� ����� ����� ������� (��� �����). � �������� ���������
#��������������� �� ����������������. �������������, ��� ������������� ���������� ������������ �� ��������.
########################################################################################################

#������������ ���������
MTSS.pdf <- density(rMTSS, bw = "ucv") #probability density function
lines(MTSS.pdf) 
#���������� �������� ������
rug(rMTSS)

#������������ ������� �������������
MTSS.�df <- ecdf(rMTSS) #Cumulative distribution function
plot(MTSS.�df, do.points = FALSE, verticals = TRUE)

#��������� � ���������������� ��������������
erMTSS <- mean(rMTSS)
stMTSS <- sd(rMTSS)
plot(MTSS.�df, do.points = FALSE, verticals = TRUE)
lines(bins,pnorm(bins, mean = erMTSS, sd = sd(rMTSS)), lty = 3) 
#������������� �� ���������

#������ ����������������� 
qqplot(rnorm(n = 10^5, mean = erMTSS, sd = stMTSS), rMTSS) 
abline(0,1) 
#�����s �������, ������ ����� �������.

#�������� �� ������������
# �����������
# if the p-value is less than the chosen alpha level, then the null hypothesis is rejected
shapiro.test(rMTSS)
#���������� �������� ������������ p-value = 4.556e-10 ��� alpha = 0.05
 
# ��������������������
ks.test(unique(rMTSS), "pnorm", mean = erMTSS, sd = stMTSS) #������� �������������
#���������� �������� ������������ p-value = 0.03655 ��� alpha = 0.05
