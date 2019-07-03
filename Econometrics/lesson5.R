getwd()
setwd("C:/Users/Svetlana/OneDrive/Эконометрическое моделирование/ДЗ3")
file.show("infdata.txt")
dat <- read.table("infdata.txt", sep = "", dec = ",", header = TRUE)
dat
qplot(x=dat$y, y=dat$inf, data = dat)
isolmod <- lm(data=dat, inf~y); summary(isolmod)
modinf1 <- lm(data = dat,inf~+m+mr+y)
summary(modinf1)
inf_m_mr <- lm(data = dat,inf~+m+mr)
summary(inf_m_mr)
y_m_mr <- lm(data = dat,y~+m+mr)
summary(y_m_mr)
#очистка inf от влияния m и mr
refinf <- residuals(inf_m_mr)
#аналогично вычисляем очищенные значения y
refy <- residuals(y_m_mr)
qplot(x = refy, y = refinf)
extdat <- data.frame(dat, refinf, refy)
#частная модель
parmod1 <- lm(data = extdat, refinf~refy)
summary(parmod1)
