set.seed(1234)

alpha = 1.5   # shape
beta = 1    # scale	

n = 50
t=rweibull(n, alpha, beta)
y=sort(t, decreasing = FALSE)
l<-length(y)
k=head(y,35)
m<-length(k)
tk<-k[35]

vec<-k
weibull_loglik=function(parm){
n<-length(vec)
gamma<-parm[1]
lambda<-parm[2]
lik<-(-exp(-gamma*tk/lambda))^(l-m)*prod((-exp(-gamma*vec/lambda)*gamma*(vec/lambda)^(gamma-1))/lambda)
loglik<-log(lik)
return(-loglik)
}
y.mle<-nlm(weibull_loglik,parm<-c(0.1,0.1))
y.mle
#cdf plots fitted weibull model
l<-c(0.,0.061,0.122,0.183,0.244,0.305,0.366,0.427,0.488,0.549,0.61,0.671,0.732,0.793,0.854,0.915,0.976,1.037,1.098,1.159,1.22,1.281,1.342,1.403,1.464,1.525,1.586,1.647,1.708,1.769,1.83,1.891,1.952,2.013,2.074,2.135,2.196,2.257,2.318,2.379,2.44,2.501,2.562,2.623,2.684,2.745,2.806,2.867,2.928,2.989)
p<-c(1/50,1/25,3/50,2/25,1/10,3/25,7/50,4/25,9/50,1/5,11/50,6/25,13/50,7/25,3/10,8/25,17/50,9/25,19/50,2/5,21/50,11/25,23/50,12/25,1/2,13/25,27/50,14/25,29/50,3/5,31/50,16/25,33/50,17/25,7/10,18/25,37/50,19/25,39/50,4/5,41/50,21/25,43/50,22/25,9/10,23/25,47/50,24/25,49/50,1)
plot(y,p, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",)
par(new=TRUE)
plot(function(vec) pweibull(vec, shape = 1/y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",axes=FALSE)

