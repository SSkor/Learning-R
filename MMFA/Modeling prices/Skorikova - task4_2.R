#Скорикова ПМ3-3#################################################################################
#Время выполнения от 15 минут
#кодировка CP1251
library(ghyp)
SBER <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/SBER_170101_180101.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                 header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)]
rSBER <- diff(SBER)/SBER[-length(SBER)]
mean(rMTSS); mean(rSBER)
lenrMTSS <- length(rMTSS)
# доходности портфеля из двух активов
prt <- array(c(rMTSS, rSBER), dim = c(lenrMTSS, 2))

#оценка параметров модели
prt.fit <- fit.ghypmv(prt, symmetric = FALSE, silent = TRUE)

alpha <- 0.1; N <- 10^6
T1 <- 201; T2 <- lenrMTSS - T1
h <- round(0.8*lenrMTSS) 
flag <- 0
kupics <- c()
lopeses <- c()
blankas <- c()
#Оптимизация портфеля
riskparameters <- c("sd", "value.at.risk", "expected.shortfall")
optmethod <- c("minimum.risk", "tangency", "target.return")
ws <- c("x1", "x2")
?portfolio.optimize
for (k in riskparameters){
  for (l in optmethod) {
    flag <- flag + 1
    opt <- portfolio.optimize(prt.fit,
                          risk.measure = k,type = l,
                          target.return = 0.0007, risk.free = 0.00009, level = 0.95, silent = TRUE )
    w <- opt$opt.weights
    ws <- data.frame(ws, w)
    VaR <- numeric()
    ?portfolio.optimize
    for (i in (T1+1):(T1+T2)) {
      h.portfolio <- prt[(i-h):(i-1), ] #обучающая выборка
      prt.fit <- fit.ghypmv(h.portfolio, symmetric = TRUE, silent = TRUE)
      sim <- rghyp(n = N, object = prt.fit)
      prt.sim <- w[1] * sim[,1] + w[2] * sim[,2] #доходности портфеля
      prt.sim <- sort(prt.sim)
      VaR[i-T1] <- prt.sim[alpha * N]
    }
    fact <- w[1] * prt[,1] + w[2] * prt[,2]
    fact <- fact[(T1+1):(T1+T2)]
    plot(fact, type="l", main = paste(k,l))
    lines(VaR, col = "red" )
    #тест Купика (kupiec test)
    K <- sum(fact < VaR) 
    alpha0 <- K/T2 #доля пробитий
    S <- -2 * log((1 - alpha)^(T2 - K) * alpha^K)+
    2 * log((1 - alpha0)^(T2 - K) * alpha0^K)
    kupics[flag] <- 1 - pchisq(S, df = 1)
    #Функции потерь
    lopeses[flag] <- sum((fact - VaR)^2 * (fact < VaR))/K 
    blankas[flag] <- sum((fact - VaR)/VaR * (fact < VaR))/K 
    print(flag*100/9) #индикатор выполнения цикл 1..9
  }
}
Lo <- matrix(lopeses,3,3)
Pk <- matrix(kupics,3,3)
Lb <- matrix(blankas,3,3)
givenames <- function(x){
  rownames(x, do.NULL = TRUE, prefix = "row")
  rownames(x) <- riskparameters
  colnames(x, do.NULL = TRUE, prefix = "col")
  colnames(x) <- optmethod
  return(x)
}
givenames(Lo); givenames(Pk); givenames(Lb)
ws
kupics 
lopeses 
blankas
#ВЫВОД###########################################################################################
#Веса портфелей x1 0.4673444 #0.2583261 # 1.2342151 0.455276 #0.2583261 # 1.2342151 0.455276 #0.2583261 # 1.2342151
############### x2 0.5326556 #0.7416739 #-0.2342151 0.544724 #0.7416739 #-0.2342151 0.544724 #0.7416739 #-0.2342151
#При оптимизации методами "tangency" и "target.return" независимо от меры риска портфели получаются одинаковыми.
#В случае "minimum.risk" одинаковые портфели при мерах "value.at.risk" и "expected.shortfall"
#Итак, получили 4 разных портфеля. (0.4673444, 0.5326556), (0.2583261, 0.7416739), (1.2342151, -0.2342151)
#и (0.455276, 0.544724)
#Обратим внимание, что получились одинаковые портфели при одинаковых мерах риска. Эта особенность характерна
#для симметричных распределений (справка по функции)
###################Лопеса########################################################################
#                   minimum.risk     tangency target.return
#sd                 0.0002471449 0.0002376318  0.0002372961
#value.at.risk      0.0001073574 0.0001078607  0.0001074313
#expected.shortfall 0.0009391906 0.0009386911  0.0009399595
####################P-значения Купика############################################################
#                   minimum.risk tangency target.return
#sd                     0.311892 0.311892      0.311892
#value.at.risk          0.311892 0.311892      0.311892
#expected.shortfall     1.000000 1.000000      1.000000
####################Бланко-Ила###################################################################
#                   minimum.risk  tangency target.return
#sd                    0.9402905 0.9270549     0.9256211
#value.at.risk         0.6346501 0.6383917     0.6360380
#expected.shortfall    1.1876535 1.1859887     1.1874714
#################################################################################################
