## sampling from a Weibull distribution with parameters
## shape=1.5 and scale=1.0
x.wei<-rweibull(n=200,shape=1.5,scale=1.0)
## theorical quantiles from a Weibull population with
## known paramters shape=1.5 e scale=1.0
x.teo<-rweibull(n=200,shape=1.5,scale=1.0)
## QQ-plot abline(0,1) a 45-degree reference line is
## plotted
qqplot(x.teo,x.wei,main="QQ-plot distr. Weibull")
curve(dweibull(x, scale=1.0, shape=1.5),from=0, to=3, main="Weibull distribution")