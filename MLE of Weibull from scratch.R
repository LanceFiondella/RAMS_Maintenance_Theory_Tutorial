set.seed(1234)

alpha = 1.5   # shape
beta = 1    # scale	

n = 50
x=c(0.0807362,0.147014,0.211721,0.290214,0.292867,0.365921,0.420821,0.431629,0.440675,0.473263,0.500056,0.524718,0.654139,0.663968,0.689502,0.697818,0.707392,0.712452,0.720935,0.770856,0.775773,0.780275,0.784847,0.872561,0.88858,0.900682,0.907473,0.968562,0.977094,0.99633,1.03369,1.05816,1.17297,1.17814,1.17994,1.19921,1.26996,1.35861,1.39173,1.43148,1.43293,1.61969,1.71256,1.7394,1.76644,1.96121,2.12992,2.13882,2.50073,2.58441)
y=sort(x, decreasing = FALSE)
l<-length(y)
k=head(y,30)
m<-length(k)
T<-1
logL = function(x){
     -sum((pweibull(k, shape = x[1], scale = x[2], log = TRUE)))*(-dweibull(T, shape = x[1], scale = x[2], log = TRUE))**(l-m)
}

y.mle = nlm(logL, c(shape = 0.1, scale = 0.1), hessian = TRUE)
y.mle

vec<-y
weibull_loglik=function(parm){
n<-length(vec)
gamma<-parm[1]
lambda<-parm[2]
loglik<-n*gamma*log(lambda)+n*log(gamma)+(gamma-1)*sum(log(vec))-sum((vec*lambda)^gamma)
return(-loglik)}
weibull<-nlm(weibull_loglik,parm<-c(1,1), hessian=TRUE)
l<-c(0.,0.061,0.122,0.183,0.244,0.305,0.366,0.427,0.488,0.549,0.61,0.671,0.732,0.793,0.854,0.915,0.976,1.037,1.098,1.159,1.22,1.281,1.342,1.403,1.464,1.525,1.586,1.647,1.708,1.769,1.83,1.891,1.952,2.013,2.074,2.135,2.196,2.257,2.318,2.379,2.44,2.501,2.562,2.623,2.684,2.745,2.806,2.867,2.928,2.989)
# pdf and cdf plots fitted weibull model
p<-c(1/50,1/25,3/50,2/25,1/10,3/25,7/50,4/25,9/50,1/5,11/50,6/25,13/50,7/25,3/10,8/25,17/50,9/25,19/50,2/5,21/50,11/25,23/50,12/25,1/2,13/25,27/50,14/25,29/50,3/5,31/50,16/25,33/50,17/25,7/10,18/25,37/50,19/25,39/50,4/5,41/50,21/25,43/50,22/25,9/10,23/25,47/50,24/25,49/50,1)
plot(vec,p,axes=FALSE)
par(new=TRUE)
plot(function(l) pweibull(l, shape = weibull$estimate[1], 
      scale = 1/weibull$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")
hist(vec)
par(new=TRUE)
plot(function(vec) dweibull(vec, shape = weibull$estimate[1], 
      scale = 1/weibull$estimate[2]), 0, 3, xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")

