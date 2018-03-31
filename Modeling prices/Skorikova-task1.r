#Скорикова ПМ3-3########################################################################################
set.seed(0) #для повторяемости 

MTSS <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)] #вектор доходностей, MTSS[-length(MTSS)] drops the last element of a

RTSI <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/RI.RTSI_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rRTSI <- diff(RTSI)/RTSI[-length(RTSI)]

#CML
rf <- 0.05 #безрисковая ставка
erRTSI <- mean(rRTSI)
stRTSI <- sd(rRTSI)
curve( stRTSI/(erRTSI-rf)*(x-rf), from = 0, to = 1, 
       xlab = "Expected Returns", ylab = "Volatility", main = "Capital Market Line") #у линии тупой наклон

#stock's
#Гистограмма доходностей MTSS
rMTSSmin <- min(rMTSS)
rMTSSmax <- max(rMTSS)
bins <- seq(rMTSSmin, rMTSSmax, length.out = 20) #карманы
hist(rMTSS, breaks = bins, prob=TRUE) 

#ДОБАВЛЕНО 29.03.18#####################################################################################
#гистограмма островершинная, слева длинный хвост, распределение не симметричное. Посмотрим на статистики
library(fBasics) #ДОБАВЛЕНО 29.03.18
kurtosisMTSS <- basicStats(rMTSS)["Kurtosis",]
skewnessMTSS <- basicStats(rMTSS)["Skewness",]
#У нормального распределения куртозис = 3, а коэфф.асимметрии = 0. У распределения доходностей МТС Skewness = -0.809858
#Kurtosis = 6.034835. Отрицательная асимметрия свидетельствует о том, что левый хвост длиннее (или толще). А значение куртозиса
#свидетельствует об островершинности. Следовательно, для моделирования нормальное распределние не подходит.
########################################################################################################

#эмпирическая плотность
MTSS.pdf <- density(rMTSS, bw = "ucv") #probability density function
lines(MTSS.pdf) 
#добавление исходных данных
rug(rMTSS)

#эмпирическая функция распределения
MTSS.сdf <- ecdf(rMTSS) #Cumulative distribution function
plot(MTSS.сdf, do.points = FALSE, verticals = TRUE)

#Сравнение с затабулированным распределением
erMTSS <- mean(rMTSS)
stMTSS <- sd(rMTSS)
plot(MTSS.сdf, do.points = FALSE, verticals = TRUE)
lines(bins,pnorm(bins, mean = erMTSS, sd = sd(rMTSS)), lty = 3) 
#распределения не совпадают

#график квантиль–квантиль 
qqplot(rnorm(n = 10^5, mean = erMTSS, sd = stMTSS), rMTSS) 
abline(0,1) 
#хвостs тяжелые, причем левый тяжелее.

#проверка на нормальность
# Шапиро–Уилка
# if the p-value is less than the chosen alpha level, then the null hypothesis is rejected
shapiro.test(rMTSS)
#отклонение гипотезы нормальности p-value = 4.556e-10 при alpha = 0.05
 
# Колмогорова–Смирнова
ks.test(unique(rMTSS), "pnorm", mean = erMTSS, sd = stMTSS) #удалила повторяющиеся
#отклонение гипотезы нормальности p-value = 0.03655 при alpha = 0.05
