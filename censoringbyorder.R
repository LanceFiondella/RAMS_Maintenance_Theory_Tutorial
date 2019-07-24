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
logL = function(x){
     -dweibull(tk, shape =x[1], scale = x[2], log =TRUE)**(l-m)*(-sum((pweibull(k, shape = x[1], scale = x[2], log = TRUE))))
}

y.mle = nlm(logL, c(shape = 0.1, scale = 0.1), hessian = TRUE)
y.mle

vec<-k
weibull_loglik=function(parm){
n<-length(vec)
gamma<-parm[1]
lambda<-parm[2]
total=0
for (i in 1:n) {
total<-total-gamma*vec/lambda+log(gamma)+((gamma-1)*log(vec/lambda))/log(lambda)
}
loglik<--(l-m)*log(1-exp(-gamma*tk/lambda))+total
return(-loglik)}
weibull<-nlm(weibull_loglik,parm<-c(0.1,0.1), hessian=TRUE)

# pdf and cdf plots fitted weibull model
plot(ecdf(y), xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",axes=FALSE)
par(new=TRUE)
plot(function(k) pweibull(k, shape = y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")


