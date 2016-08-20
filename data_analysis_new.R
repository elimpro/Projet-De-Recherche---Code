
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
library(sde)  ##Simulating SDE
library(Sim.DiffProc) # Estimating SDE
library(TTR)
#######################

#### S&P 500

# read data 

#google
#apple
#microsoft
#total

a <- ZigZag(price.close, change = 6, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)
plot(price.close,main = "Example 1 - CORN")
lines(a,col = 'red')
lines(resistance,col= 'blue')
lines(resistance,col= 'darkgreen', type= 'S')



tot = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/New_data/microsoft.csv",header=FALSE,sep = ',')

date <- as.Date(tot$V1,"%Y-%m-%d") # For apple and Corn use . 
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
price.close <- xts(tot$V5, order.by = Date) # For apple and Corn use V6

plot(price.close,main = "Closing price MICROSOFT",ylab = "price",xlab ="time")
#dygraph(price.close)

day <- price.close
percent = 2
levels <- ZigZag(day, change = percent, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)
plot(day,main = "ZigZag plot")
#lines(levels,pch=22, lty=1,col = 134)
#Other type of plot

#chart_Series(day)
#add_TA(ZigZag(day,0.1),on=1)

#####Getting the support and resistance :

i = findPeaks(levels)
j = findPeaks(-levels)

resistance <- levels[i-1]
support <- levels[j-1]

lines(support,type = "s",col = "red")
lines(resistance,type = "s",col = "blue")



### Support & Resistant,

zigzagtotal = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/New_data/zigzagmicrosoft.csv",header=FALSE,sep = ',')

# Get i for resistance 
i = which(zigzagtotal$V4== 1)
j = which(zigzagtotal$V4== -1)

# Resistance levels 

date = zigzagtotal$V1[i]
p = zigzagtotal$V5[i]
res <- data.frame(date,p)
date <- as.Date(res$date,"%Y.%m.%d")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
resistance <- xts(res$p, order.by = date)
#Small S or big S
lines(resistance,type = 'S',col= 'blue')

### Support levels :


date = zigzagtotal$V1[j]
p = zigzagtotal$V5[j]
sup <- data.frame(date,p)
date <- as.Date(sup$date,"%Y.%m.%d")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
supp <- xts(sup$p, order.by = date)
#Small S or big S
lines(supp,type = 'S',col='red')

###########

data <- sup$p[155:170]
data <- res$p[129:150]
data <- sup$p[114:124]


data <- sup$p[154:187]

# google

data <- sup$p[162:174]
data <- sup$p[151:161]
data <- res$p[124:138]


#Corn

data <- res$p[185:204]
data <- sup$p[156:166]
data <- res$p[111:121]
data <- res$p[19:38]

hist(data,10)
mean(data)

#data <- res$p
plotdist(data, histo = TRUE, demp = TRUE)

# to understand this graph : http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
descdist(data, boot = 1000)

fw <- fitdist(data, "lnorm")
fg <- fitdist(data, "norm")
fe <- fitdist(data,"lnorm")
summary(fe)
summary(fw)
plot(fw)


####### Goodness of fit Plot

par(mfrow = c(2, 2))
plot.legend <- c("Gamma", "normal","lognorm")
denscomp(list(fw, fg,fe), legendtext = plot.legend)
qqcomp(list(fw, fg,fe), legendtext = plot.legend)
cdfcomp(list(fw, fg,fe), legendtext = plot.legend)
ppcomp(list(fw, fg,fe), legendtext = plot.legend)

gofstat(list(fg, fw,fe), fitnames = c("gamma", "norm","lnorm"))

##############################################################

#### Estimate SDE 

# price.close -- > sp$V6
### Total 
Exp1
i = 2079
j = 2271

Exp2

27/10/2008 --> 27/10/2009
j = 2002
i = 1741

exp3
11-01-2010 --> 12-07-2010

i = 1559
j = 1689

#### Google 
exp1
first 6month --> 11/06/2008

i = 2031
j =2135


exp2

24/06/2008 --> 26/01/2009

i = 1872
j = 2020


exp3

08/09/2009--> 08/09/2010
j = 1716
i = 1465


#Corn

exp1:

debut to 12/09/2007

i = 1
j = 228


exp2 :
  
08/10/2008 --> 08/04/2009
i =499
j =625


exp3 : 
  
  17/01/2011 -> 17/06/2011

i = 1092
j = 1212

exp4:
  1/10/2014 -> 01/10/2015

  i = 2114
  j = 2348

D <- paste(tot$V1[i:j])
Date=as.POSIXct(strptime(D,"%Y.%m.%d"))
price.close <- xts(tot$V6[i:j], order.by = Date)
plot(price.close)
#################### Estimations
## Begin

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
X  <- ts(tot$V6[i:j],frequency = 60)   # j:i et V for the others sauf apple et corn : i:j et v6 !!!
plot(X)
fit <- mle(BS.lik, start=list(theta=1, sigma=1), method="L-BFGS-B",lower=c(-3,0.00001))
coef(fit)
length(X)*deltat(X) 

#################### End

###### Begin partie fitsde.pdf

X  <- ts(tot$V5[j:i],frequency = 30)

fx <- expression( theta[1]*x ) ## drift coefficient of model (9)
gx <- expression( theta[2]*x ) ## diffusion coefficient of model (9)

# mode : euler , shoji , kessler
fitmod <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))

coef(fitmod)
summary(fitmod)

##### Ploting

#########
#completed price
# Only for (After april 2016)
date <- as.Date(suite$Date,"%Y-%m-%d")
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
price.s <- xts(suite$Close, order.by = Date)
price <- rbind(price.close,price.s)
##########


######Plot All :
exp6 = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/plots_AllResults/corn/4.txt",header=FALSE)
i = 2114
j = 2348+80
freq = 60
l = 2348
k = 2348+freq -1
a = vector("numeric", freq)
a <- rev(exp6$V1)
D <- paste(tot$V1[l:k],tot$V2[l:k],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
level1 <- xts(a, order.by = Date)
D <- paste(tot$V1[i:j],tot$V2[i:j],sep = " ")
Date=as.POSIXct(strptime(D,"%Y.%m.%d %H:%M"))
price.close <- xts(tot$V6[i:j], order.by = Date)
plot(price.close)
lines(resistance,type = "S",col="forestgreen")
abline(v=.index(price.close)[l-i+1], col="red")
lines(level1,col = "darkviolet")


##### The case of total, google

exp6 = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/plots_AllResults/google/3.txt",header=FALSE)

j = 1716
i = 1465-100

freq = -60
l = 1465
k = 1465+freq+1
a = vector("numeric", -freq)
a <- rev(exp6$V1)
D <- paste(tot$V1[l:k],sep = " ")
Date=as.POSIXct(strptime(D,"%Y-%m-%d "))
level1 <- xts(a, order.by = Date)
D <- paste(tot$V1[i:j],sep = " ")
Date=as.POSIXct(strptime(D,"%Y-%m-%d "))
price.close <- xts(tot$V5[i:j], order.by = Date)
plot(price.close)
lines(resistance,type = "S",col="forestgreen")
abline(v=.index(price.close)[j-l+1], col="red")
lines(level1,col = "darkviolet")



#### High frequency data

D <- paste(data_1s$V1,data_1s$V2,data_1s$V3,data_1s$V4,data_1s$V5,data_1s$V6,sep = " ")
Date=as.POSIXct(strptime(D,"%Y %m %d %H %M %S"))
sp500 <- xts(data_1s$V8, order.by = Date)
plot(sp500)
dygraph(sp500)

#see : http://nbviewer.jupyter.org/github/jbn/ZigZag/blob/master/zigzag_demo.ipynb
#http://www.r-bloggers.com/a-better-zigzag/

D <- paste(data_60s_excel$V2,sep = " ")
Date=as.POSIXct(strptime(D,"%d %m %Y %H:%M"))
sp500_ <- xts(data_60s_excel$V1, order.by = Date)
plot(sp500_)

