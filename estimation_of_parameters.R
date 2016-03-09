#### Header ####
# Estiamtion of parameters
# Adapted from Hohl http://www.statistik.lmu.de/~hoehle/teaching/moid2011/moid.pdf

#### Load Libraries ####
require(ggplot2)

#### Likelihood function for Reed-Frost model
# Parameters:
#   w.logit - logit(w) to have unrestricted parameter space
#   x       - vector containing the number of susceptible people at each time
#   y       - vector contining the number of infectious people at each time

lik_RF_fcn <- function(w.logit, x, y){
  if (length(x) != length(y)){
    stop("x and y need to be the same length")
  }
  
  K <- length(x)
  w <- plogis(w.logit)
  theta <- 1 - (1-w)^y
  
  return(sum(dbinom(y[-1], size = x[-K], prob = theta[-K], log=TRUE)))
}

#### Test likelihood function for Reed-Frost model
y <- c(1,4,5,6,4,0)
x <- c(20,16,11,5,1,1)
t <- seq(1:length(x))
data <- data.frame(cbind(t, x, y))
ggplot(data, aes(x = t)) + geom_line(aes(y=x), color = "blue") + geom_line(aes(y=y), color = "red") + theme_bw()
mle <- optim(par = 0, fn = lik_RF_fcn, method = "BFGS", x = x, y = y, control = list(fnscale = -1), hessian = TRUE)
(w.had <- plogis(mle$par)) # MLE estimator
(w.p5ci <- plogis(mle$par + c(-1,1)*qnorm(0.975)*sqrt(-1/mle$hess)))
