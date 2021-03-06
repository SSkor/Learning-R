#��������� ��3-3###############################################################################
#��������� CP1251
library(ghyp)
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
lenrMTSS <- length(rMTSS)

# ��������� ������ ����� � ������
fact <- rMTSS[(T1+1):(T1+T2)]
plot(fact, type = "l")

#������ VaR 80%################################################################################
alpha <- 0.1
T1 <- 201; T2 <- lenrMTSS - T1
VaR <- numeric()
h <- round(0.8*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
lines(VaR, col = "red")

#���� ������ (kupiec test)
K <- sum(fact < VaR) #5 ��������
alpha0 <- K/T2 #���� ��������
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1)
#P = 1. � ������ �� ��������� ������� ��������. 
#� ������ VaR ������ ������.

#������� ������ ������ 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.0006419035
#������� ������-���
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #1.018963

#������ VaR 50%################################################################################
VaR <- numeric()
h <- round(0.5*lenrMTSS)  # ����� ��������� �������
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #�������� ������ ������������� �������
}
lines(VaR, col = "blue")

#���� ������ (kupiec test)
K <- sum(fact < VaR) #6 ��������
alpha0 <- K/T2 #���� ��������
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1)
#P = 0.64. � ������ �� ��������� ������� ��������. 
#�� ���������� �������� ������� ���������� �� ����������, ��� ��� ��������� ������� 80% 

#������� ������ ������ 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.0006188573
#������� ������-���
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #1.324841

#�����#########################################################################################
#1. � ����������� ������ ��������� �������, p-value �����������. ������, ��������� ����� �������� � ����������� �����������.
#2.1. �.�. ������ Var ���������� �� ��� ������, �� ���� (������� ������ ������) �� �������� ����� ������������ ����������, 
# ������� �������� ��������.� ��������� ������ �� ���������.
#2.2. ���� ������-��� ����� ������������ ��������� ������. ������� �������� ���������� ������������.