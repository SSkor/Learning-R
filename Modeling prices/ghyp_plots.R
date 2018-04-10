#Playing with parameters 
library("ghyp")
#Generalised hyperbolic distribution
## lambda/chi parametrization of a univariate GH distribution
xx <- seq(-10,10,0.01)
plot1 <- ghyp(lambda=2, chi=1, psi=0.5, mu=0, sigma=1, gamma=0)
plot2 <- ghyp(lambda=0.1, chi=1, psi=0.5, mu=0, sigma=1, gamma=0)
plot3 <- ghyp(lambda=2, chi=100, psi=0.5, mu=0, sigma=1, gamma=0)
plot4 <- ghyp(lambda=2, chi=1, psi=0.5, mu=0, sigma=1, gamma=-0.5)
plot5 <- ghyp(lambda=2, chi=1, psi=0.01, mu=0, sigma=1, gamma=0)
#PDF
plot(plot1, range = qghyp(xx, plot1), type="l", xlab="x value", ylab="Density", 
     main="Generalised hyperbolic distribution", col="blue")
lines(plot2, range = qghyp(xx, plot2), col = "red")
lines(plot3, range = qghyp(xx, plot3), col = "plum")
lines(plot4, range = qghyp(xx, plot4), col = "green")
lines(plot5, range = qghyp(xx, plot5), col = "orange")

#X ??? GHd(??, ??, ??, µ, ??, ??)
#chi отвечает за остроту графика, чем меньше, тем островершиненней 
#?? is the skewness parameter
#µ is the location parameter
#?? = AA??? is the dispersion-matrix
# ??, ??, ?? determine the shape of the distribution. That is, how much weight is
# assigned to the tails and to the center. In general, the larger those
# parameters the closer the distribution is to the normal distribution.
#lambda за хвостатость