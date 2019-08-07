set.seed(1234)
### Generate random values for Weibull Distribution
alpha<- 1.5   # shape
beta<- 1    # scale	
n<- 50
x<- rweibull(n, alpha, beta)

### Sorting the random values for Weibull Distribution
y<- sort(x, decreasing = FALSE)

### Taking a group of elements from the total population for censoring
l<-length(y)
k<- head(y,35)
m<-length(k)
tk<-k[35]

### Log-likelihood function for censoring 
vec<-k
weibull_loglik=function(parm){
n<-length(vec)
gamma<-parm[1]
lambda<-parm[2]
lik<-(-exp(-gamma*T/lambda))^(l-m)*prod((-exp(-gamma*vec/lambda)*gamma*(vec/lambda)^(gamma-1))/lambda)
loglik<-log(lik)
return(-loglik)
}
y.mle<-nlm(weibull_loglik,parm<-c(0.1,0.1))
y.mle

#cdf plots fitted Weibull Distribution
time<-seq(0, 3, by=0.061) ###Random time units within the desired range
fc<-seq(0.02, 1, by=0.02) ###Random failure counts
plot(y,fc, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")
par(new=TRUE)
plot(function(time) pweibull(time, shape = 1/y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",axes=FALSE)

