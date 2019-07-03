#Скорикова ПМ3-3###############################################################################
#кодировка CP1251
library(ghyp)
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
lenrMTSS <- length(rMTSS)

# сравнение оценок риска с фактом
fact <- rMTSS[(T1+1):(T1+T2)]
plot(fact, type = "l")

#Кривая VaR 80%################################################################################
alpha <- 0.1
T1 <- 201; T2 <- lenrMTSS - T1
VaR <- numeric()
h <- round(0.8*lenrMTSS)  # длина обучающей выборки
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #квантиль лучшей аппроксимации выборки
}
lines(VaR, col = "red")

#тест Купика (kupiec test)
K <- sum(fact < VaR) #5 пробитий
alpha0 <- K/T2 #доля пробитий
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1)
#P = 1. А значит не отвергаем нулевую гипотезу. 
#А значит VaR найден хорошо.

#Функция потерь Лопеса 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.0006419035
#Функция Бланко-Ила
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #1.018963

#Кривая VaR 50%################################################################################
VaR <- numeric()
h <- round(0.5*lenrMTSS)  # длина обучающей выборки
for (i in (T1+1):(T1+T2)) {
  h.MTSS <- rMTSS[(i-h):(i-1)]
  MTSS.fit <- stepAIC.ghyp(h.MTSS,dist=c("gauss","t","ghyp"), silent=TRUE)
  VaR[i-T1] <- qghyp(alpha, object=MTSS.fit$best.model) #квантиль лучшей аппроксимации выборки
}
lines(VaR, col = "blue")

#тест Купика (kupiec test)
K <- sum(fact < VaR) #6 пробитий
alpha0 <- K/T2 #доля пробитий
S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
  2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
p.value <- 1 - pchisq(S, df = 1)
#P = 0.64. А значит не отвергаем нулевую гипотезу. 
#Но количество пробитий сильнее отличается от ожидаемого, чем при обучающей выборке 80% 

#Функция потерь Лопеса 
L.L0 <- sum((fact - VaR)^2 * (fact < VaR))/K #0.0006188573
#Функция Бланко-Ила
L.BI <- sum((fact - VaR)/VaR * (fact < VaR))/K #1.324841

#ВЫВОД#########################################################################################
#1. С уменьшением объема обучающей выборки, p-value уменьшается. Значит, ожидаемое число пробитий с фактическим уменьшается.
#2.1. Т.к. кривая Var изменяется не так сильно, то тест (функция потерь Лопеса) не выявляет новые значительные отклонения, 
# поэтому меняется несильно.А маленькие пробои не учитывает.
#2.2. тест Бланко-Ила лучше обнаруживает небольшие пробои. Поэтому значение изменяется значительнее.