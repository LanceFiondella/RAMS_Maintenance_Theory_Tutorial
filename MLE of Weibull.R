### Maximum Likelihood Estimates for the Weibull Distribution
library(plotly)
set.seed(1234)

### Generate random values for Weibull Distribution
alpha<- 1.5   # shape
beta<- 1    # scale	
n<- 50
x<- rweibull(n, alpha, beta)

### Sorting the random values for Weibull Distribution
y<- sort(x, decreasing = FALSE)

# plot the simulated data
plot(density(y))
rug(y)

# log-Likelihood function
logL = function(x){
      -sum(dweibull(y, shape = x[1], scale = x[2], log = TRUE))
}

y.mle = nlm(logL, c(shape = 1.5, scale = 1), hessian = TRUE)
y.mle

# cdf and pdf plots fitted Weibull Distribution
plot(ecdf(y))
par(new=TRUE)
plot(function(y) pweibull(y, shape = y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, max(y), xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")

hist(y)
par(new=TRUE)
plot(function(y) dweibull(y, shape = y.mle$estimate[1], 
      scale = y.mle$estimate[2]), 0, max(y), xlab = "lifetimes", 
      ylab = "density", main = "fitted Weibull density using MLE")

rug(y)

# contour plot of maximum likelihood estimates
len = 101
grid = matrix(0.0, nrow = len, ncol = len)
shape.vals = seq(0.01,3.5, len = len)
scale.vals = seq(0.01, 3.5, len = len)

for(i in seq(along = shape.vals)){
      for(j in seq(along = scale.vals)){
            grid[i,j] = logL(c(shape.vals[i],scale.vals[j]))
      }
}
contour(shape.vals, scale.vals, grid, levels = 50:100)
points(y.mle$estimate[1],y.mle$estimate[2], pch = "+")
title(main = "log-Likelihood contours, Weibull", xlab = expression(alpha), 
      ylab = expression(beta))







