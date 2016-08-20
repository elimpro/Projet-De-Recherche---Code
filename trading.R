
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
require(btutils)
require(quantmod)  # library for finance

###ZigZag mode
s <- getSymbols('rdwr', auto.assign=FALSE)
chart_Series(s, subset="2015-07::")
add_TA(ZigZag(s[,2:3],5),on=1)
A <- ZigZag(s[,2:3],5)



####Corn data

tot = read.csv("/Users/imadelhanafi/Desktop/pre/DATA/trading/corn.csv",header=FALSE,sep = ',')
date <- as.Date(tot$V1,"%Y.%m.%d") # For apple and Corn use . 
Date=as.POSIXct(strptime(date,"%Y-%m-%d"))
data <- xts(tot$V5, order.by = Date) # For apple and Corn use V6
plot(data)

### High frequency data data - 1s

data_hf <- read.csv("/Users/imadelhanafi/Desktop/pre/DATA/trading/data_hf.csv",header=FALSE,sep = ',')
D <- paste(data_hf$V1,data_hf$V2,data_hf$V3,data_hf$V4,data_hf$V5,data_hf$V6,sep = " ")
Date=as.POSIXct(strptime(D,"%Y %m %d %H %M %S"))
data <- xts(data_hf$V8, order.by = Date)
plot(data)


############### 1min Data 

data <- read.csv("/Users/imadelhanafi/Desktop/pre/DATA/trading/data.csv",header=FALSE,sep = ',')
D <- paste(data$V2,sep = " ")
Date=as.POSIXct(strptime(D,"%d/%m/%Y %H:%M"))
pl <- ts(data$V1)
plot(pl, main = "S&P close price on 15days - Frequency 1min", ylab = "Close price")
data <- xts(data$V1, order.by = Date)


# data for one day 
# Day : mean a period of time !!! where we will do 4 trades

#please choose a day  (1 to 13)
#Parameters
# There's 4 period in each d

byday = 500 # How much values in one d
d <- 1
percent = 1 # The percent for ZigZag indicator
freq = 125  # Frequency of each period ==> Number of observation in 1period of 1D
# Attention : 4*freq should be << byday 


begin = (d-1)*byday+1
end =d*byday
plot(data[begin:end])
day <- data[begin:end]

#### Plot the zigzag filter 

levels <- ZigZag(day, change = percent, percent = TRUE, retrace = FALSE, lastExtreme = TRUE)
plot(day,main = "ZigZag plot")
lines(levels,pch=22, lty=1,col = 134)
#Other type of plot

#chart_Series(day)
#add_TA(ZigZag(day,0.1),on=1)

#####Getting the support and resistance :

i = findPeaks(levels)
j = findPeaks(-levels)

resistance <- levels[i-1]
support <- levels[j-1]

lines(support,type = "s",col = "blue")
lines(resistance,type = "s",col = "skyblue")

########## Estimating the parameters on first period 

# Everu 90min : 90 value

X  <- ts(day[1:freq],frequency = freq)
fx <- expression( theta[1]*x ) ## drift coefficient of model (9)
gx <- expression( theta[2]*x ) ## diffusion coefficient of model (9)
# mode : euler , shoji , kessler
fitmod1 <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))


s = freq
u = 2*freq
X  <- ts(day[s:u],frequency = freq)
fx <- expression( theta[1]*x ) ## drift coefficient of model (9)
gx <- expression( theta[2]*x ) ## diffusion coefficient of model (9)
# mode : euler , shoji , kessler
fitmod2 <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))


s = 2*freq
u = 3*freq
X  <- ts(day[s:u],frequency = freq)
fx <- expression( theta[1]*x ) ## drift coefficient of model (9)
gx <- expression( theta[2]*x ) ## diffusion coefficient of model (9)
# mode : euler , shoji , kessler
fitmod3 <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))

s = 3*freq
u = 4*freq
X  <- ts(day[s:u],frequency = freq)
fx <- expression( theta[1]*x ) ## drift coefficient of model (9)
gx <- expression( theta[2]*x ) ## diffusion coefficient of model (9)
# mode : euler , shoji , kessler
fitmod4 <- fitsde(data=X,drift=fx,diffusion=gx,start = list(theta1=1,theta2=1),pmle="shoji",lower=c(-10,0.0001))


abline(v=.index(day)[freq], col="red")
abline(v=.index(day)[2*freq], col="red")
abline(v=.index(day)[3*freq], col="red")
abline(v=.index(day)[4*freq], col="red")



####### Getting supp & rest only for 
m1 <- fitmod1$coef[1]
m2 <- fitmod2$coef[1]
m3 <- fitmod3$coef[1]
m4 <- fitmod4$coef[1]
### Period 1

v=index(day)[1]
w = index(day)[freq]

a = sprintf("%s/%s",v,w)
if (m1 > 0) {
  res1 <- coredata(resistance[a])
  cat("Period1: mu := ",fitmod1$coef[1],"; Sc :=",fitmod1$coef[2],";\n")
  cat("Resistance \n")
  cat("x=[",res1,"]") }

if (m1 < 0) {
  supp1 <- coredata(support[a])
  cat("Period1: mu := ",fitmod1$coef[1],"; Sc :=",fitmod1$coef[2],";\n")
  cat("Support \n")
  cat("x=[",supp1,"]") }



##### Period 2


v=index(day)[freq]
w = index(day)[2*freq]

a = sprintf("%s/%s",v,w)
if (m2 > 0) {
  res2 <- coredata(resistance[a])
  cat("Period2: mu := ",fitmod2$coef[1],"; Sc :=",fitmod2$coef[2],";\n")
  cat("Resistance \n")
  cat("x=[",res2,"]") }

if (m2 < 0) {
  supp2 <- coredata(support[a])
  cat("Period2: mu := ",fitmod2$coef[1],"; Sc :=",fitmod2$coef[2],";\n")
  cat("Support \n")
  cat("x=[",supp2,"]") }

#### Period 3


v=index(day)[2*freq]
w = index(day)[3*freq]

a = sprintf("%s/%s",v,w)
if (m3 > 0) {
  res3 <- coredata(resistance[a])
  cat("Period3: mu := ",fitmod3$coef[1],"; Sc :=",fitmod3$coef[2],";\n")
  cat("Resistance \n")
  cat("x=[",res3,"]") }

if (m3 < 0) {
  supp3 <- coredata(support[a])
  cat("Period3: mu := ",fitmod3$coef[1],"; Sc :=",fitmod3$coef[2],";\n")
  cat("Support \n")
  cat("x=[",supp3,"]") }

###period4

v=index(day)[3*freq]
w = index(day)[4*freq]

a = sprintf("%s/%s",v,w)
if (m4 > 0) {
  res4 <- coredata(resistance[a])
  cat("Period4: mu := ",fitmod4$coef[1],"; Sc :=",fitmod4$coef[2],";\n")
  cat("Resistance \n")
  cat("x=[",res4,"]") }

if (m4 < 0) {
  supp4 <- coredata(support[a])
  cat("Period4: mu := ",fitmod4$coef[1],"; Sc :=",fitmod4$coef[2],";\n")
  cat("Support \n")
  cat("x=[",supp4,"]") }


######Plot All

# To plot bornes 




# To plot curve




##### Distribution

data <- supp4[,1]
hist(data)
mean(data)
#data <- res$p
plotdist(data, histo = TRUE, demp = TRUE)

# to understand this graph : http://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm
descdist(data, boot = 1000)
ge <- gevmle(data)
ge
l <- rgev(10000,ge[1],  ge[2],  ge[3])
hist(l)
mean(l)



fw <- fitdist(data, "gamma",start = c(1,1))
fe <- fitdist(data,"lnorm",start = c(1,1))
summary(fe)
summary(fw)
plot(fw)
dygraph(day)

################################## Plot ###############################


byday = 500 # How much values in one d
d <- 5 #
percent = 1 # The percent for ZigZag indicator
freq = 125  # Frequency of each period ==> Number of observation in 1period of 1D
# Attention : 4*freq should be << byday 


begin = (d-1)*byday+1
end =d*byday
plot(data[begin:end])
day <- data[begin:end]

plot(day)
abline(v=.index(day)[freq], col="red")
abline(v=.index(day)[2*freq], col="red")
abline(v=.index(day)[3*freq], col="red")
abline(v=.index(day)[4*freq], col="red")
lines(support,type = "s",col = "blue")
lines(resistance,type = "s",col = "skyblue")


txt <- rev(d3.2$V1)
a = vector("numeric", 125)

for(j in 1:125)
{
  if(j%%2 == 1)
  {
    a[j] = txt[(j+1)/2]
  }
  else
  {
    a[j] = 0
  }
}

for(j in 1:125)
{
  if(j%%2 == 0)
  {
    a[j] = (a[j-1]+a[j+1])/2
  }
}

a[124] = a[123]-(a[122]-a[123])
a[125] = a[124]-(a[123]-a[124])

#oh = 2*freq
#eh = freq+1
oh = 3*freq
eh = 2*freq+1
D <- paste(index(day)[eh:oh])
Date=as.POSIXct(strptime(D,"%Y-%m-%d"))
level1 <- xts(a, order.by = Date)
lines(level1,col='orange')



txt2 <- rev(d5.3$V1)
b = vector("numeric", 125)

for(j in 1:125)
{
  if(j%%2 == 1)
  {
    b[j] = txt2[(j+1)/2]
  }
  else
  {
    b[j] = 0
  }
}

for(j in 1:125)
{
  if(j%%2 == 0)
  {
    b[j] = (b[j-1]+b[j+1])/2
  }
}

oh = 4*freq
eh = 3*freq+1
D <- paste(index(day)[eh:oh])
Date=as.POSIXct(strptime(D,"%Y-%m-%d"))
level2 <- xts(b, order.by = Date)
lines(level2,col='orange')
#####################

###Plot for constant

c = vector("numeric", 125)
d= vector("numeric", 125)

for(j in 1:125)
{
 c[j] = 612.919

 d[j] =  572.829
}

oh = 2*freq
eh = freq+1
D <- paste(index(day)[eh:oh])
Date=as.POSIXct(strptime(D,"%Y-%m-%d"))
level3 <- xts(c, order.by = Date)
lines(level3,col='orange')
level4 <- xts(d, order.by = Date)
lines(level4,col='orange')

