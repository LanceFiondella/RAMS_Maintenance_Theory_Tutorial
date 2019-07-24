set.seed(1234)

alpha = 1.5   # shape
beta = 1    # scale	

n = 50
x=rweibull(n, alpha, beta)
y=sort(x, decreasing = FALSE)
l<-length(y)
k=head(y,35)
m<-length(k)
T<-0.89
logL = function(x){
     -sum((pweibull(k, shape = x[1], scale = x[2], log = TRUE)))*(1-dweibull(T, shape = x[1], scale = x[2], log = TRUE))**(l-m)
}

y.mle = nlm(logL, c(shape = 0.1, scale = 0.1), hessian = TRUE)
y.mle
# pdf and cdf plots fitted weibull model
plot(ecdf(y),xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",axes=FALSE)
par(new=TRUE)
plot(function(y) pweibull(y, shape = y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")


