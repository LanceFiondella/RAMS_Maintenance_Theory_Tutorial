### Maximum Likelihood Estimates for the Weibull Distribution using formula
set.seed(1234)

### Generate random values for Weibull Distribution
alpha<- 1.5   # shape
beta<- 1    # scale	
n<- 50
x<- rweibull(n, alpha, beta)

### Sorting the random values for Weibull Distribution
y<- sort(x, decreasing = FALSE)

### Log-likelihood function using function
logL = function(x){
     -sum((pweibull(k, shape = x[1], scale = x[2], log = TRUE)))*(-dweibull(T, shape = x[1], scale = x[2], log = TRUE))**(l-m)
}

y.mle = nlm(logL, c(shape = 0.1, scale = 0.1), hessian = TRUE)
y.mle

### Log-likelihood function using formula
vec<-y
weibull_loglik=function(parm){
n<-length(vec)
gamma<-parm[1]
lambda<-parm[2]
loglik<-n*gamma*log(lambda)+n*log(gamma)+(gamma-1)*sum(log(vec))-sum((vec*lambda)^gamma)
return(-loglik)}
weibull<-nlm(weibull_loglik,parm<-c(1,1), hessian=TRUE)

# pdf and cdf plots fitted weibull model
l<-c(seq(0, 3, by = 0.061))  ###Random failure counts
p<-c(seq(0.02, 1, by = 0.020)) ###Random time units within the desired range
plot(vec,p,axes=FALSE)
par(new=TRUE)
plot(function(l) pweibull(l, shape = weibull$estimate[1], 
      scale = 1/weibull$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")
hist(vec,axes=FALSE)
par(new=TRUE)
plot(function(vec) dweibull(vec, shape = weibull$estimate[1], 
      scale = 1/weibull$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")

