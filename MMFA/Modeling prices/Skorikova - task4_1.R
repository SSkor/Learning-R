#��������� ��3-3#################################################################################
#��������� CP1251
library(ghyp)
SBER <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/SBER_170101_180101.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
rSBER <- diff(SBER)/SBER[-length(SBER)]
lenrMTSS <- length(rMTSS)
# ���������� �������� �� ���� �������
prt <- array(c(rMTSS, rSBER), dim = c(lenrMTSS, 2))
?array

#������ ���������� ������
#���������� ������ - ���������
prt.fit <- fit.ghypmv(prt, symmetric = FALSE, silent = TRUE)
aic.mv <- stepAIC.ghyp(prt, dist = c("t","ghyp"), symmetric = NULL, silent = TRUE )
summary(aic.mv$best.model)
#�� ������ - Symmetric Student-t Distribut


alpha <- 0.1; N <- 10^6
#������ �����
w <- c(0.5,0.5)  # ���� ������� � ��������
sim <- rghyp(n = N, object = prt.fit)
prt.sim <- w[1] * sim[,1] + w[2] * sim[,2]
prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha * N]
ES <- mean(prt.sim[1:(alpha*N-1)])

#����������� ��������
opt <- portfolio.optimize(prt.fit,
                          risk.measure="value.at.risk",type="minimum.risk",
                          target.return=NULL,risk.free=NULL,level=0.95,silent=TRUE)
w <- opt$opt.weights

T1 <- 201; T2 <- lenrMTSS - T1
VaR <- numeric()
h <- round(0.8*lenrMTSS)  # ����� ��������� �������
#i <- T1+1
N <- 10^6
for (i in (T1+1):(T1+T2)) {
  h.portfolio <- prt[(i-h):(i-1), ] #��������� �������
  prt.fit <- fit.ghypmv(h.portfolio, symmetric = TRUE, silent = TRUE)
  #prt.fit <- stepAIC.ghyp(h.portfolio, dist=c("t","ghyp"),symmetric=NULL,silent=TRUE )$best.model #�� ��������
  sim <- rghyp(n = N, object = prt.fit)
  prt.sim <- w[1] * sim[,1] + w[2] * sim[,2] #���������� ��������
  prt.sim <- sort(prt.sim)
  VaR[i-T1] <- prt.sim[alpha * N]
  print(round((i-T1)/T2, 4)*100) #�������� ����������
}
fact <- w[1] * prt[,1] + w[2] * prt[,2]
plot(fact[(T1+1):(T1+T2)],type="l")
lines(VaR, col = "red")

fact<-fact[(T1+1):(T1+T2)]

#���� ������ (kupiec test)
K <- sum(fact < VaR) #4 ��������
alpha0 <- K/T2 #���� ��������
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1) #0.6264461 VaR ��������� ������


#������� ������ ������ 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.000205441
#������� ������-���
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #0.741464

