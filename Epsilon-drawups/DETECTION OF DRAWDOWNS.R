##Epsilon Drawup/Down Methodology for Determination of Potential Bubble Peaks##
## DATA INTERVAL ----
# e.g. start-6000th observation, end-10000th obs.
torgi <- traddays[, 2]
####################
#[1]"Power law scaling and “Dragon-Kings”
#in distributions of intraday financial drawdowns" Vladimir Filimonov,
#Didier Sornette, 2015 (pg.5)
#script by S.Skorikova (2018)
## DEFINE FUNCTION EPSILON.METHOD ----
# volatility formula is from https://www.r-bloggers.com/what-is-volatility/
epsilon.method <- function(rt, eps0, w) {
  rt <- diff(log(torgi))
  n <- length(rt)
  i <- 1
  comsumm <- cumsum(rt[(w + 1):n])
  ii1 <- 0
  saveii1 <- c()
  while (ii1 + i != (n + 1 - w)) {
    if (rt[ii1 + 1 + w] < 0)
      d <- comsumm[i] - min(comsumm[1:i])
    if (rt[ii1 + 1 + w] > 0)
      d <- max(comsumm[1:i]) - comsumm[i]
    eps <- eps0 * sd(rt[(ii1 + i):(ii1 + i + (w - 1))]) * sqrt(w)
    if (d > eps) {
      if (rt[ii1 + 1 + w] < 0)
        i1 <- which.min(comsumm[1:i])
      if (rt[ii1 + 1 + w] > 0) {
        i1 <- which.max(comsumm[1:i])
        saveii1 <- c(saveii1, ii1 + i1)
      }
      ii1 <- ii1 + i1
      newrt <- rt[(ii1 + 1 + w):n]
      comsumm <- cumsum(newrt)
      i <- 0
    }
    i <- i + 1
  }
  return(saveii1 + w)
}
## EXAMPLE ----
# eps0 <- 1
# w <- 60
# peaks <- epsilon.method(rt, eps0, w)
# rt[peaks] * rt[peaks + 1]
# plot(torgi, type = "l")
# abline(v = peaks + 1)
# abline(v = w, col = "red")
####################
#[2] "Dissection of Bitcoin’s Multiscale Bubble History from January 2012 to February 2018"
#J.C. Gerlach, G. Demos, D. Sornette, 2018 (pg.27,28)
#script by S.Skorikova (2018)
## GET ALL POTENTIAL PEAKS USING EPSILON.METHOD ----
tau.get <- function(rt, eps0.set, w.set) {
  tau <- c()
  pb = txtProgressBar(
    min = 0,
    max = length(eps0.set) * length(w.set),
    initial = 0
  )
  pb.counter <- 0
  for (eps0 in eps0.set) {
    for (w in w.set) {
      tau <- c(tau, epsilon.method(rt, eps0, w))
      pb.counter <- pb.counter + 1
      setTxtProgressBar(pb, pb.counter)
    }
  }
  return(tau)
}
## EXAMPLE ----
w.set <- seq(10, 60, 5)
eps0.set <- seq(0.1, 5, 0.1) #default values according [2]
tau <- tau.get(rt, eps0.set, w.set)
## GET Ntpk series (frequency of each potential peak) ----
N.epsilon <- length(eps0.set) * length(w.set)
Ntpk <- table(as.data.frame(tau)) / N.epsilon
#View(Ntpk) #run me if you want to see the table
## PEAK'S TEST ----
DLT <- 0.95
DST <- 0.65 #default values according [2]
TLT.table <- Ntpk[Ntpk > DLT]
TSL.table <- Ntpk[DLT >= Ntpk & Ntpk > DST]
## FINAL DETECTED LONG AND SHORT BUBBLE'S PEAKS ----
TLT <- as.numeric(names(TLT.table))
TSL <- as.numeric(names(TSL.table))
## FINAL PLOT ----
plot(torgi, type = "l")
abline(v = TLT + 1)