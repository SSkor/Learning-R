library(zoo)
##1ST STEP ----
lppl_estimate_rob_1s=function(x, par_start = c(bet=0.5, ome=6, A=log(x[NROW(x)]), B=0, C1=0, C2=0), max.win.tc=0.1){

	# Compose function to be MINIMIZED:
	LPPL_LLH1 = function(parm){
		bet = parm[1]; ome = parm[2]; A = parm[3]; B = parm[4]; C1=parm[5]; C2=parm[6];
		n=NROW(x)       # number of observations
		sdum=rep(1,n)
		tt_sim=seq(1, n, 1)
		tc=NROW(x)+max.win.tc*NROW(x)
		f_t=(tc - tt_sim)^bet;
		g_t=( (tc - tt_sim)^bet )*cos( ome*log(tc - tt_sim) )
		h_t=( (tc - tt_sim)^bet )*sin( ome*log(tc - tt_sim) )
		Y=log(x)
		et=Y-sdum*A- B*f_t - C1*g_t - C2*h_t
		et=coredata(et)
		ll=sum(et*et)
 	}

	# Estimate Parameters:
	 fit = nlminb(par_start, objective=LPPL_LLH1, control = list(trace=0), hessian=TRUE)

	par_est1=c( (fit$par) )
	return(par_est1)
}
##2ND STEP ----
lppl_estimate_rob_2s=function(x,par, max.win.tc=0.1){

	#  Create the starting value for the critical time tc:
	par_start = c(tc=NROW(x)+max.win.tc*NROW(x))

	#  Compose function to be MINIMIZED:
	LPPL_LLH2 = function(parm){
		tc=parm[1]
		bet = par[1]; ome = par[2]; A = par[3]; B = par[4]; C1=par[5]; C2=par[6];
		n=NROW(x)       # number of observations
		sdum=rep(1,n)
		tt_sim=seq(1, n, 1)
		f_t=(tc - tt_sim)^bet
		g_t=( (tc - tt_sim)^bet )*cos( ome*log(tc - tt_sim) )
		h_t=( (tc - tt_sim)^bet )*sin( ome*log(tc - tt_sim) )
		Y=log(x)
		et=Y-sdum*A- B*f_t - C1*g_t - C2*h_t
		et=coredata(et)
		ll=sum(et*et)
 	}

	#Estimate Parameters:
  fit = nlminb(par_start, objective=LPPL_LLH2, control = list(trace=0), hessian=TRUE)

	par_est2=c( (fit$par) )
	return(par_est2)
}
##3RD STEP ----—ÃŒ“–» ›“Œ“ ÿ¿√ √ƒ≈ “”“ NAN
lppl_estimate_rob_3s=function(x,par1,par2){

	# Create the starting values for the parameter vector:
	par_start = c(par1,par2)

	# Compose function to be MINIMIZED:
	LPPL_LLH3 = function(parm){
		bet = parm[1]; ome = parm[2]; A = parm[3]; B = parm[4]; C1=parm[5]; C2=parm[6]; tc =parm[7];
		n=NROW(x)      # number of observations
		sdum=rep(1,n)
		tt_sim=seq(1, n, 1)
		f_t=(tc - tt_sim)^bet
		g_t=( (tc - tt_sim)^bet )*cos( ome*log(tc - tt_sim) )
		h_t=( (tc - tt_sim)^bet )*sin( ome*log(tc - tt_sim) )
		Y=log(x)
		et=Y-sdum*A- B*f_t - C1*g_t - C2*h_t
		et=coredata(et)
		ll=sum(et*et)
 	}

	# Estimate Parameters:
	  fit = nlminb(par_start, objective=LPPL_LLH3, control = list(trace=0), hessian=TRUE)

	#Computation of residuals and KPSS test statistic
	bet = fit$par[1]; ome = fit$par[2]; A = fit$par[3]; B = fit$par[4]; C1=fit$par[5]; C2=fit$par[6]; tc =fit$par[7];
		n=NROW(x)
		sdum=rep(1,n)
		tt_sim=seq(1, n, 1)
		f_t=(tc - tt_sim)^bet
		g_t=( (tc - tt_sim)^bet )*cos( ome*log(tc - tt_sim) )
		h_t=( (tc - tt_sim)^bet )*sin( ome*log(tc - tt_sim) )
		Y=log(x)
		et=Y-sdum*A- B*f_t - C1*g_t - C2*h_t
		SSE.N <- mean(et*et) 

	par_est=c( (fit$par), SSEN = SSE.N )
	return(par_est)
}
## THE LAGRANGE REGULARISATION APPROACH FOR THE DETERMINATION OF BUBBLE START TIME ----
LBorSB <- LB[[7]]
LBorSB.tail <- LB[[8]]
LBorSB.size <- length(LBorSB)
start.d <- 30
edn.d <-50
windows.set <- start.d:edn.d 
SSE.N <- c()
pb = txtProgressBar(min = 0, max = edn.d-start.d+1, initial = 0)
pb.counter <- 0
## Õ≈ «¿¡€“‹ Œ¡–¿¡Œ“¿“‹ NAN 
for (win in windows.set){
  x <- c(LBorSB[(LBorSB.size-win+1):LBorSB.size], LBorSB.tail[1:round(0,3*win)])
  est.step1 <- lppl_estimate_rob_1s(x)
  est.step2 <- lppl_estimate_rob_2s(x, est.step1)
  est.step3 <- lppl_estimate_rob_3s(x, est.step1, est.step2)
  SSE.N <- c(SSE.N, est.step3["SSEN"]) 
  pb.counter <- pb.counter + 1
  setTxtProgressBar(pb, pb.counter)
}
plot(SSE.N)
xx <- 1:length(SSE.N)
slope <- coef(lm(SSE.N~xx))[2]
trend <- slope*xx
SSE.N.detrended <- SSE.N-trend
plot(SSE.N.detrended)

