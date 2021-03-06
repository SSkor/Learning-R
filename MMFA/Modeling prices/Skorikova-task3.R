library(ghyp)
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
lenrMTSS <- length(rMTSS)

#������ VaR 80%
alpha <- 0.1
T1 <- 201; T2 <- lenrMTSS - T1
VaR <- numeric()
h <- round(0.8*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
# ��������� ������ ����� � ������
fact <- rMTSS[(T1+1):(T1+T2)]
plot(fact,type="l")
lines(VaR,col="red")

#������ VaR 70%
VaR <- numeric()
h <- round(0.7*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
lines(VaR,col="green")

#������ VaR 50%
VaR <- numeric()
h <- round(0.5*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
lines(VaR,col="blue")

#������ VaR 30%
VaR <- numeric()
h <- round(0.3*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
lines(VaR,col="darkmagenta")
#����� ��� ��������� ��������� ������� ������ �� ���������� ��������� ������������� � ������������� ���������� Var
#��� ��������� ���������� ������ Var ����� ���������