vdata <- c(10, -10, 0, -10, 20, -20, 0, -20, 50)
V <- solve(matrix(data=vdata, nrow=3, ncol=3)) #îáğàòíàÿ ìàòğèöà solve
I <- rep(1, 3) #âåêòîğ-ñòîëáåö
mu <- c(12, 13, 12) #âåêòîğ-ñòîëáåö
a <- as.numeric(t(I)%*%V%*%I) #àëüôà
b <- as.numeric(t(I)%*%V%*%mu) #áåòà
g <- as.numeric(t(mu)%*%V%*%mu) #ãàììà
d <- as.numeric(a*g-b^2) #äåëüòà
#MVF <- (a*m^2-2*b*m+g)/d
curve(((a*x^2-2*b*x+g)/d)^(1/2),0,100)
mumin <- b/a
l <- (g-b*mumin)/d
t <- (a*mumin-b)/d
X <- V%*%(l*I+t*mu)
qumin <- 1/sqrt(a)