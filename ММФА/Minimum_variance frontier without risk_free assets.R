library(data.table)
library(tseries)
#Efficient Frontier with Short-Selling
setwd("C:/Users/SSkorikova/Desktop")
ROSN <- read.csv("ROSN.csv", header = TRUE, sep=",")[["Change.."]][1:21] #доходности в %
MGNT <- read.csv("MGNT.csv", header = TRUE, sep=",")[["Change.."]][1:21]
MTSS <- read.csv("MTSS.csv", header = TRUE, sep=",")[["Change.."]][1:21]
tickers <- data.frame(ROSN, MGNT, MTSS)
vdata <- cov(tickers)
V <- solve(vdata) #обратная матрица solve
I <- rep(1, 3) #вектор-столбец
mu <- apply(tickers, 2, mean)
a <- as.numeric(t(I)%*%V%*%I) #альфа
b <- as.numeric(t(I)%*%V%*%mu) #бета
g <- as.numeric(t(mu)%*%V%*%mu) #гамма
d <- as.numeric(a*g-b^2) #дельта
curve( ((a*x^2-2*b*x+g)/d)^(1/2), from=-1, to=1, xlab ="Expected Returns", ylab = "Volatility", 
       main = "Efficient Frontier with Short-Selling") #MVF
mumin <- min(mu)
mumax <- max(mu)
l <- (g-b*mumin)/d
t <- (a*mumin-b)/d
X <- V%*%(l*I+t*mu)
qumin <- 1/sqrt(a)
#without Short-Selling
er_vals<- seq(from = mumin, to = mumax, length.out = 1000)
sd_vals <- sapply(er_vals, function(er) {
  op <- portfolio.optim(as.matrix(tickers), er)
  return(op$ps)
})
plot_dt <- data.table(er = er_vals, sd = sd_vals)
plot(plot_dt, main = "Efficient Frontier without Short-Selling", xlab ="Expected Returns", ylab = "Volatility")
#CML

