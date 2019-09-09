rm(list=ls())
2+2
2*4
1/4
4^2
MyData <-read.csv("PA_Data.csv")
str(MyData)
head(MyData)
tail(MyData)
seq(1:100)
seq(1,100,0.5)
x = seq(1:100)
mean(x)
y = runif(100, min = 0, max = 10)
########
# PLOTS
########
plot(x,y)
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5)
par(mai=c(0.85,0.9,0.25,0.05))
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5)
par(mfrow=c(1,2))
w <- runif(100, min = 0, max = 10)
par(mfrow=c(1,2))
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers 1", cex=1.5, cex.lab=1.5)
plot(x,w,main="My Figure", xlab="Sequence", ylab="Random Numbers 2", cex=1.5, cex.lab=1.5)
par(mfrow=c(1,1))
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", cex=1.5, cex.lab=1.5, col="blue")
points(x,w, cex=1.5, col="red")
par(mfrow=c(1,1))
plot(x,y,main="My Figure", xlab="Sequence", ylab="Random Numbers", ylim=c(0,12), cex=1.5, cex.lab=1.5, col="blue")
points(x,w, cex=1.5, col="red")
legend("topleft", legend=c("Random Numbers 1", "Random Numbers 2"), pch=21, pt.bg=c("Blue", "Red"), pt.cex=1.5)
#############
# DATA FRAMES
#############
DataData <- data.frame(Column1=c(2,4,6,8), Column2=seq(1:4), Column3=c(8,6,4,2))
DataData$Column4 <- c(0.1,0.2,0.3,0.4)
DataData$Column5 <- DataData$Column1 * DataData$Column4

###########
# FOR LOOPS
###########

Data_Loop <-NULL
i <- 0.5
for(j in seq(0.1,1,0.1)){
loop_1 <- i + j
Data_Loop <- rbind(Data_Loop, data.frame(Parameterj=j, Result=loop_1))
print(j)
}

Data_Loop[3,2]

Data_Loop$Parameterj

############
# FUNCTIONS
############

FOO <- function(s,q){
	(s + q)}
FOO(1,1)
Data_Loop2 <-NULL
Data_Loop2 <- data.frame(Parameterq=seq(0.1,1,0.1), Result=FOO(0.5,seq(0.1,1,0.1)))
