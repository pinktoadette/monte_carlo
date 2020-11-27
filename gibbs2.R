
gibbs<- function(p, rho, iter){
	N <- vector()
	proposed=rnorm(p)
	for (t in 1:iter){
		for (j in 1:p){
			m=sum(proposed[-j])/(p-1)
			mean = (p-1)*rho*m/(1+(p-2)*rho)
			sd = sqrt((1+(p-2)*rho-(p-1)*rho^2)/(1+(p-2)*rho))
			proposed[j]=rnorm(1,mean,sd)
		}
		N =cbind(N,proposed)
	}	
	par(mfrow=c(1,5))
	for (i in 1:p){
		hist(N[i,],prob=TRUE,xlab=paste0("Giibs ",i),main="")
		curve(dnorm(x),add=TRUE,lwd=1)
	}
}


start_time <- Sys.time()
gibbs(5, 0.25, 500)
end_time <- Sys.time()

print (end_time - start_time)