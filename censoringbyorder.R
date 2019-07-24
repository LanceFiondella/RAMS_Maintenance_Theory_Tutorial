set.seed(1234)

alpha = 1.5   # shape
beta = 1    # scale	

n = 50
t=c(0.0807362,0.147014,0.211721,0.290214,0.292867,0.365921,0.420821,0.431629,0.440675,0.473263,0.500056,0.524718,0.654139,0.663968,0.689502,0.697818,0.707392,0.712452,0.720935,0.770856,0.775773,0.780275,0.784847,0.872561,0.88858,0.900682,0.907473,0.968562,0.977094,0.99633,1.03369,1.05816,1.17297,1.17814,1.17994,1.19921,1.26996,1.35861,1.39173,1.43148,1.43293,1.61969,1.71256,1.7394,1.76644,1.96121,2.12992,2.13882,2.50073,2.58441)
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


