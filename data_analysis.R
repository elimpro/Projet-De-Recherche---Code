
library(lattice)
library(MASS)
library(forecast)
library(dygraphs)
library(zoo)
library(timeDate)
library(xts)
library(stats)
library(mgcv)
require(utils)
require(timeDate)
library(fitdistrplus) # To fit data 
library(SpatialExtremes)  # GEV 
 
#######################

#### S&P 500

# read data as a time serie

sp = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/sp.csv",header=FALSE,sep = ',')
date <- as.Date(sp$V1,"%Y.%m.%d")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
price.close <- xts(sp$V6, order.by = Date)
plot(price.close)
dygraph(price.close)

### Support & Resistant,

spzigzag = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/spzigzag.csv",header=FALSE,sep = ',')

# Get i for resistance 
i = which(spzigzag$V4== 1) # V4 = 1 => means resistance
j = which(spzigzag$V4== -1) # indices for support

### Resistance levels as a time serie

date = spzigzag$V1[i]
p = spzigzag$V5[i]
res <- data.frame(date,p)
date <- as.Date(res$date,"%Y.%m.%d %H:%M")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
resistance <- xts(res$p, order.by = date)
lines(resistance,type = 'S')

### Support levels as a time serie

date = spzigzag$V1[j]
p = spzigzag$V5[j]
sup <- data.frame(date,p)
date <- as.Date(sup$date,"%Y.%m.%d %H:%M")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
supp <- xts(sup$p, order.by = date)
lines(supp,type = 'S')

### Resistance levels for a choosen period

data <- res$p[1:46] 
hist(data,10)
mean(data)

#data <- res$p
plotdist(data, histo = TRUE, demp = TRUE)

# to understand this graph : http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
descdist(data, boot = 1000)

fw <- fitdist(data, "gamma")
fg <- fitdist(data, "norm")
fe <- fitdist(data,"lnorm")
summary(fe)
summary(fw)
summary(fg)

####### Goodness of fit Plot

par(mfrow = c(2, 2))
plot.legend <- c("Gamma", "normal","lognorm")
denscomp(list(fw, fg,fe), legendtext = plot.legend)
qqcomp(list(fw, fg,fe), legendtext = plot.legend)
cdfcomp(list(fw, fg,fe), legendtext = plot.legend)
ppcomp(list(fw, fg,fe), legendtext = plot.legend)

gofstat(list(fg, fw,fe), fitnames = c("gamma", "norm","lnorm"))

##############################################################

############### Estimate SDE #################

# price.close -- > sp$V6

### test indexes : 
2014 -> 2016 
i <- 2765
j <- 3470

D <- paste(sp$V1[i:j],sp$V2[i:j],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
price.close <- xts(sp$V6[i:j], order.by = Date)
plot(price.close)

#

dcBS <- function(x, t, x0, theta, log = TRUE){
  ml <- log(x0) + (theta[1]-theta[2]^2/2)*t
  sl <- sqrt(t)*theta[2]
  lik <- dlnorm(x, meanlog = ml, sdlog = sl, log=TRUE)
  if(!log)
    lik <- exp(lik)
  lik }

BS.lik <- function(theta ,sigma) {
  n <- length(X)
  dt <- deltat(X)
  -sum(dcBS(x=X[2:n], t=dt, x0=X[1:(n-1)], theta=c(theta,sigma), log=TRUE)) }

# Choose frequency// Don't forget 1T !!

X  <- ts(sp$V6[i:j],frequency = 30)
plot(X)
fit <- mle(BS.lik, start=list(theta=1, sigma=1), method="L-BFGS-B",lower=c(-3,0.00001))
coef(fit)

length(X)*deltat(X) 

###### Other method using fitdist library

X  <- ts(sp$V6[i:j],frequency = 30)

fx <- expression( theta[1]*x ) ## drift coefficient of model 
gx <- expression( theta[2]*x ) ## diffusion coefficient of model 

# mode : euler , shoji , kessler
fitmod <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))
coef(fitmod)
summary(fitmod)

##### Ploting


###### Values of Resistance

lev <- rev(b$V1)
b = vector("numeric", 150)
b <- lev

j = 877
k = 877+149
D <- paste(sp$V1[j:k],sp$V2[j:k],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
level1 <- xts(b, order.by = Date)

######Plot All :

i = 528
j = 689+60
freq = 30
l = 689
k = 689+freq -1
a = vector("numeric", freq)
a <- rev(exp6$V1)
D <- paste(sp$V1[l:k],sp$V2[l:k],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
level1 <- xts(a, order.by = Date)
D <- paste(sp$V1[i:j],sp$V2[i:j],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
price.close <- xts(sp$V6[i:j], order.by = Date)
plot(price.close)
lines(supp,type = "S",col="forestgreen")
abline(v=.index(price.close)[l-i+1], col="red")
lines(level1,col = "darkviolet")


