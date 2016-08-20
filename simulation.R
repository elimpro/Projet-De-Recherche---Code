#########
library(lattice)
library(MASS)
library(forecast)
library(dygraphs)
library(zoo)
library(timeDate)
library(xts)
library(sde)  ##Simulating SDE
library(Sim.DiffProc) # Estimating SDE
library(stats)
library(mgcv)
library(rgl) # 3D plot
require(utils)
##############

#Simulation of a GBM

hx <- dnorm(x)
# GMB Parameters
r <- 1 # drift
sigma <- 2 # volatility
x=1 # starting point

######
 N <- 1000 # number of end-points of the grid including T
 T <- 1 # length of the interval [0,T] in time units
 Delta <- T/N # time increment
 W <- numeric(N+1) # initialization of the vector W
 t <- seq(0,T, length=N+1)
 for(i in 2:(N+1))
   W[i] <- W[i-1] + rnorm(1) * sqrt(Delta)
 
 #plot(t,W, type="l", main="Standard Brownian motion" , ylim=c(-1,1))
 
 S <- x * exp((r-sigma^2/2)*t + sigma*W)
 plot(t,S,type="l",main="geometric Brownian motion")
 
 
############## Simulations using SDE package
 
#set.seed(123) 

d <- expression(1*x)
s <- expression(0.5*x)
sde.sim(X0=1,drift=d, sigma=s,N = 100) -> X
plot(X,main="geometric Brownian Motion")
 
###############Estimating SDE PARAMETERS


dcBS <- function(x, t, x0, theta, log = TRUE){
  ml <- log(x0) + (theta[1]-theta[2]^2/2)*t
  sl <- sqrt(t)*theta[2]
  lik <- dlnorm(x, meanlog = ml, sdlog = sl, log=TRUE)
  if(!log)
    lik <- exp(lik)
  lik }


BS.lik <- function(theta1 ,theta2) {
  n <- length(X)
  dt <- deltat(X)
  -sum(dcBS(x=X[2:n], t=dt, x0=X[1:(n-1)], theta=c(theta1,theta2), log=TRUE)) }
##Simulating an example

 X <- sde.sim(model="BS", theta=c(.5,.2), N=1000)
 lines(X, main="Paths of a GBM")
##Estimating the parameters
 
fit <- mle(BS.lik, start=list(theta1=1, theta2=1), method="L-BFGS-B", lower=c(0.01,0.01))
coef(fit)


############# Estimating SDE using fitsde library


f <- expression( (2*x) )
 g <- expression( 0.5*x )
 
sim    <- snssde1d(drift=f,diffusion=g,x0=50,M=1,N=1000)
mydata <- sim$X
plot(mydata)

  fx <- expression( theta[1]*x ) ## drift coefficient 
  gx <- expression( theta[2]*x ) ## diffusion coefficient 
  
  fitmod <- fitsde(data=mydata,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji")
  
  coef(fitmod)
  summary(fitmod)