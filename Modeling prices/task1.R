MTSS <-read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/MTSS_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rMTSS <- diff(MTSS)/MTSS[-length(MTSS)] #вектор доходностей, MTSS[-length(MTSS)] drops the last element of a

RTSI <-read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/RI.RTSI_170101_171231.csv", 
                header=TRUE, sep=',')[["X.CLOSE."]]
rRTSI <- diff(RTSI)/RTSI[-length(RTSI)]

#stock's
rMTSSmin <- min(rMTSS)
rMTSSmax <- max(rMTSS)
bins <- seq(rMTSSmin, rMTSSmax, length.out=10) #карманы
hist(rMTSS, breaks=bins, prob=TRUE) #Гистограмма исходных данных
empiricdMTSS <- density(rMTSS, bw = "ucv")
lines(empiricdMTSS) #эмпирическая плотность
rug(rMTSS)