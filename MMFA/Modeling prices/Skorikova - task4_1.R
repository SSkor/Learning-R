#Скорикова ПМ3-3#################################################################################
#кодировка CP1251
library(ghyp)
SBER <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/SBER_170101_180101.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
rSBER <- diff(SBER)/SBER[-length(SBER)]
lenrMTSS <- length(rMTSS)
# доходности портфеля из двух активов
prt <- array(c(rMTSS, rSBER), dim = c(lenrMTSS, 2))
?array

#оценка параметров модели
#Обобщенное гиперб - Стьюдента
prt.fit <- fit.ghypmv(prt, symmetric = FALSE, silent = TRUE)
aic.mv <- stepAIC.ghyp(prt, dist = c("t","ghyp"), symmetric = NULL, silent = TRUE )
summary(aic.mv$best.model)
#по Акаику - Symmetric Student-t Distribut


alpha <- 0.1; N <- 10^6
#оценки риска
w <- c(0.5,0.5)  # веса активов в портфеле
sim <- rghyp(n = N, object = prt.fit)
prt.sim <- w[1] * sim[,1] + w[2] * sim[,2]
prt.sim <- sort(prt.sim)
VaR <- prt.sim[alpha * N]
ES <- mean(prt.sim[1:(alpha*N-1)])

#оптимизация портфеля
opt <- portfolio.optimize(prt.fit,
                          risk.measure="value.at.risk",type="minimum.risk",
                          target.return=NULL,risk.free=NULL,level=0.95,silent=TRUE)
w <- opt$opt.weights

T1 <- 201; T2 <- lenrMTSS - T1
VaR <- numeric()
h <- round(0.8*lenrMTSS)  # длина обучающей выборки
#i <- T1+1
N <- 10^6
for (i in (T1+1):(T1+T2)) {
  h.portfolio <- prt[(i-h):(i-1), ] #обучающая выборка
  prt.fit <- fit.ghypmv(h.portfolio, symmetric = TRUE, silent = TRUE)
  #prt.fit <- stepAIC.ghyp(h.portfolio, dist=c("t","ghyp"),symmetric=NULL,silent=TRUE )$best.model #не работает
  sim <- rghyp(n = N, object = prt.fit)
  prt.sim <- w[1] * sim[,1] + w[2] * sim[,2] #доходности портфеля
  prt.sim <- sort(prt.sim)
  VaR[i-T1] <- prt.sim[alpha * N]
  print(round((i-T1)/T2, 4)*100) #прогресс выполнения
}
fact <- w[1] * prt[,1] + w[2] * prt[,2]
plot(fact[(T1+1):(T1+T2)],type="l")
lines(VaR, col = "red")

fact<-fact[(T1+1):(T1+T2)]

#тест Купика (kupiec test)
K <- sum(fact < VaR) #4 пробития
alpha0 <- K/T2 #доля пробитий
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1) #0.6264461 VaR построена хорошо


#Функция потерь Лопеса 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.000205441
#Функция Бланко-Ила
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #0.741464

