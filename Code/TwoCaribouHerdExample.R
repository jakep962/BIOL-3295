# TOY EXAMPLE: TWO POPULATIONS ARE PERCEIVED AS ONE

# To run this file click "Source" in the upper left hand side
# Or enter: source('~/Desktop/BIOL-3295/Code/TwoCaribouHerdExample.R')
# insole the CONSOLE below or in base R, however you will need to know
# the correct path. Entering getwd() into the console may help you with
# determining the path.

# This is commented out. If you would like to improve the color palette
# you will need to Install the package "viridis". At the very end of this
# script there is a line you can uncomment to utilize the better color
# palette.

# Loads a color palette
# library(viridis)

# Remove all objects - always start with this
rm(list=ls())

# This function assumes geometric growth (i.e. no density dependence)
# The function will output a list of times and corresponding
# population sizes

# b: births per individual in a year
# d: prob of dying in a year
# Nstart: population size at time tstart
# tstart: the start time
# tend: the end time

GeoGrowth = function(Nstart,b,d,tstart,tend){
  Result = c(tstart,Nstart)
  N = Nstart
  for(t in seq(tstart+1,tend,1)){
    N = N + b*N - d*N # This is the core equation. It states
    # the future population size is the current population size, N,
    # + births - deaths. Then the new population size is used
    # iteratively to calculate the next year, and so forth.
    Result <- rbind(Result, data.frame(time=t, Popn.Size=N))
  } 
  return(Result)
}

# TYPE: Popn1 = GeoGrowth(490,0.5,0.95,2015,2080) INTO YOUR CONSOLE. YOU WILL
# HAVE TO HAVE RUN THE CODE FOR THE GeoGrowth FUNCTION FOR THIS TO WORK. NEXT
# TYPE Popn1 INTO THE CONSOLE. TYPE Popn1$time AND THEN Popn1$Popn.Size.

# Population 1 is the declining population. We will consider 3 scenarios
# for population 1: a) fast, b) medium, and c) slow decrease.
# The birth rate is set to 1/2 an offspring per individual per year
# and the death rate is varied to affect the rate of population decline
Popn1a = GeoGrowth(490,0.5,0.95,2015,2080)
Popn1b = GeoGrowth(490,0.5,0.6,2015,2080)
Popn1c = GeoGrowth(490,0.5,0.55,2015,2080)

# Population 2 is the increasing poulation. We will consider 3 scenarios
### a) fast, b) medium, and c) slow increase. Note the initial poulation size
#Shouldn't it be a) slow, b) medium and c) fast increase. Since a smaller death rate value will cause a faster increase in the population?
# for population 2 is just 10. What is it for population 1?
Popn2a = GeoGrowth(10,0.5,0.45,2015, 2080)
Popn2b = GeoGrowth(10,0.5,0.4,2015, 2080)
Popn2c = GeoGrowth(10,0.5,0.3,2015, 2080)

#pdf("./Fig1.pdf")
# Here we make a set of figures visualizing the dynamics of the two populations
par(mfrow = c(3,3), mar = c(4,4,1,1))
plot(Popn1a$time, Popn1a$Popn.Size, xlab = "", ylab = 'true population size', typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2a$time, Popn2a$Popn.Size, lwd=2, col='blue')

plot(Popn1a$time, Popn1a$Popn.Size, xlab = "", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500), main = "Fast decrease (popn 1)")
lines(Popn2b$time, Popn2b$Popn.Size, lwd=2, col='blue')

plot(Popn1a$time, Popn1a$Popn.Size, xlab = "", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2c$time, Popn2c$Popn.Size, lwd=2, col='blue')

plot(Popn1b$time, Popn1b$Popn.Size, xlab = "", ylab = 'true population size', typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2a$time, Popn2a$Popn.Size, lwd=2, col='blue')

plot(Popn1b$time, Popn1b$Popn.Size, xlab = "", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500), main = "Medium decrease (popn 1)")
lines(Popn2b$time, Popn2b$Popn.Size, lwd=2, col='blue')

plot(Popn1b$time, Popn1b$Popn.Size, xlab = "", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2c$time, Popn2c$Popn.Size, lwd=2, col='blue')

plot(Popn1c$time, Popn1c$Popn.Size, xlab = "year", ylab = 'true population size', typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2a$time, Popn2a$Popn.Size, lwd=2, col='blue')

plot(Popn1c$time, Popn1c$Popn.Size, xlab = "year", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500), main = "Slow decrease (popn 1)")
lines(Popn2b$time, Popn2b$Popn.Size, lwd=2, col='blue')

plot(Popn1c$time, Popn1c$Popn.Size, xlab = "year", ylab = "", typ = 'l', lwd=2,col = 'red',ylim=c(0,500))
lines(Popn2c$time, Popn2c$Popn.Size, lwd=2, col='blue')
legend("topright", legend=c("Popn 1", "Popn 2"), col=c("Blue", "Red"), lwd=2, lty = 1, box.lwd = 0)
#dev.off()

# Plot the perceived population size which is the sum of the two populations
par(mfrow = c(3,1))
plot(Popn1a$time, Popn1a$Popn.Size+Popn2a$Popn.Size, lwd = 2, col = 'black', typ= "l", ylab = "Perceived popn size", xlab = "", main = "Fast decrease (popn 1)")
lines(Popn1a$time, Popn1a$Popn.Size+Popn2b$Popn.Size, lwd = 2, col = "black", lty = 2)
lines(Popn1a$time, Popn1a$Popn.Size+Popn2c$Popn.Size, lwd = 1, col = "black")

plot(Popn1b$time, Popn1b$Popn.Size+Popn2a$Popn.Size, lwd = 2, col = 'black', typ= "l", ylab = "Perceived popn size", xlab = "", main = "Medium decrease (popn 1)")
lines(Popn1b$time, Popn1b$Popn.Size+Popn2b$Popn.Size, lwd = 2, col = "black", lty = 2)
lines(Popn1b$time, Popn1b$Popn.Size+Popn2c$Popn.Size, lwd = 1, col = "black")

plot(Popn1c$time, Popn1c$Popn.Size+Popn2a$Popn.Size, lwd = 2, col = 'black', typ= "l", ylab = "Perceived popn size", xlab = "year", main = "Slow decrease (popn 1)")
lines(Popn1c$time, Popn1c$Popn.Size+Popn2b$Popn.Size, lwd = 2, col = "black", lty = 2)
lines(Popn1c$time, Popn1c$Popn.Size+Popn2c$Popn.Size, lwd = 1, col = "black")
legend("topright", legend=c("Slow incr (popn 2)", "Medium incr (popn 2)", "Fast incr (popn 2)"), col="black", lwd=c(2,2,1), lty = c(1,2,1), box.lwd = 0)

# Vector for the d values for the contour plot
# the filled.contour function only allows for increasing x- and y-values
# (i.e. d1vec and d2vec)
d1vec = c(.55, .6, .95)
d2vec = c(.3, .4, .45)
# pre-allocate the matrix of minimum values
minarray = matrix(0,3,3)

# calculate the minimum values
minarray[1,1] = min(Popn1c$Popn.Size+Popn2c$Popn.Size)
minarray[1,2] = min(Popn1c$Popn.Size+Popn2b$Popn.Size)
minarray[1,3] = min(Popn1c$Popn.Size+Popn2a$Popn.Size)
minarray[2,1] = min(Popn1b$Popn.Size+Popn2c$Popn.Size)
minarray[2,2] = min(Popn1b$Popn.Size+Popn2b$Popn.Size)
minarray[2,3] = min(Popn1b$Popn.Size+Popn2a$Popn.Size)
minarray[3,1] = min(Popn1a$Popn.Size+Popn2c$Popn.Size)
minarray[3,2] = min(Popn1a$Popn.Size+Popn2b$Popn.Size)
minarray[3,3] = min(Popn1a$Popn.Size+Popn2a$Popn.Size)

#plot the minimum values
par(mfrow = c(1,1))
filled.contour(d1vec, d2vec, minarray, xlab = "popn 1 death rate, d", ylab = "popn 2 death rate, d", main = "Minimum perceived population size")
#filled.contour(d1vec, d2vec, minarray, xlab = "popn 1 death rate, d", ylab = "popn 2 death rate, d",color.palette=viridis, main = "Minimum perceived population size")
