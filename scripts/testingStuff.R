library(ggplot2)
library(data.table)

# Portfolio stacked columns vs. IDX
r1 <- c(1:50, 50+(1:50)/20)
r2 <- r1*0 +1
r3 <- 82-r1-r2
myMat <- data.frame( "A" = r1, "B" = r2, "C" = r3)
myMat$Idx <- 1:nrow(myMat)
myMat$varIdx <- sqrt(myMat$A)
myMat

#myMeltI <- melt(myMat[, c(1:3, 4)], id.vars = "Idx")
#myMeltV <- melt(myMat[, c(1:3, 5)], id.vars = "varIdx")
#myMeltI
#myMeltV
myMeltI <- melt(myeff.logdiff[, c(1:70, 74)], id.vars = "Idx")
myMeltV <- melt(myeff.logdiff[1:52, c(1:70, 71)], id.vars = "Std.Dev")

pSums <- data.frame(t(colSums(myeff.ret[,1:70])))
#pSums <- pSums[,order(-pSums)]
pSums
which(pSums[1,]>0.0001)
cClean <- pSums[,which(pSums[1,]>0.0001)]
cClean
myeff.clean <- myeff.ret[,c(which(pSums[1,]>0.0001), 71:74)]
nClean <- ncol(myeff.clean) -4
myeff.clean.short <- myeff.clean
myeff.clean.short$sDelta <- shift(myeff.clean$Std.Dev,1,type = "lead") - myeff.clean$Std.Dev
myeff.clean.short2 <- myeff.clean.short[which(myeff.clean.short$sDelta>1e-08),]

myeff.cleanx <- EF_CleanEFPorts( myeff.logdiff)
nClean <- ncol(myeff.cleanx) -4

myMeltI <- melt(myeff.cleanx[, c(1:nClean, nClean+4)], id.vars = "Idx")
myMeltV <- melt(myeff.cleanx[, c(1:nClean, nClean+1)], id.vars = "Std.Dev")


p <- ggplot() +  
  geom_col(data=myMeltI, aes(x=Idx,y=value,fill=variable)) +
  ggtitle(paste("Portfolios", " fred", sep = "" )) 
print(p)

p <- ggplot() +  
  geom_col(data=myMeltV, aes(x=Std.Dev,y=value,fill=variable)) +
  ggtitle(paste("Portfolios", " ginger", sep = "" )) 
print(p)

df1 <- data.frame( 
  xmin = c(1,5), 
  xmax = c(2,7), 
  ymin = c(0,3), 
  ymax = c(2,5) 
) 
ggplot(df1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect(fill="grey80") 


rep(1:4, c(2,1,2,1))
rep(1:4, c(2,1,2,1))

myeffz <- myeff.ret.zc
nTicks <- ncol(myeffz)-4
qcs <-t(apply(myeffz[,1:nTicks], 1, cumsum))
ymax <- qcs
ymin <- cbind(integer(nrow(qcs)), qcs[,1:(ncol(qcs)-1)])
nSamp <- nrow(myeffz)
xmin <- myeffz$Std.Dev
xmax <- myeffz$Std.Dev[2:nSamp]
xmax[nSamp] <- 2*xmin[nSamp] -xmin[nSamp-1]
zymin <- melt(ymin)
zymax <- melt(ymax)
zxmin <- rep(t(xmin),nTicks)
zxmax <- rep(t(xmax),nTicks)
r.df <- data.frame(xmn = zxmin, xmx = zxmax, ymn = zymin$value, ymx = zymax$value, Ticker = zymax$Var2)
p <- ggplot()
p <- p + geom_rect(data = r.df, aes(xmin=xmn, xmax=xmx, ymin=ymn,ymax=ymx,fill=Ticker), color="black")
print(p)
