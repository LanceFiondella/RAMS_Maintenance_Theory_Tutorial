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

#cdf plot fitted weibull model
time<-seq(0, 3, by=0.061)
fc<-seq(0.02, 1, by=0.02)
plot(y,fc, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")
par(new=TRUE)
plot(function(time) pweibull(time, shape = 1/y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE",axes=FALSE)



