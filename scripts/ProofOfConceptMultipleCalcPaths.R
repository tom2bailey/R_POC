# take care of situation when one ticker has less than nExpRet or nExpCovar data points. (remove that ticker?)
# calculate the eff's, and then convert to annualized, then plot

# proof of concept for different calculation types:  Daily Return, Daily Multiplier, Daily LogDiff
# proof of concept for native vs. annualized results

require(data.table)
require(zoo)
require(ggplot2)

library(data.table)
library(zoo)
library(ggplot2)



#Input Vanguard data
dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/VanguardETFStacked_2018_05.csv')
dt$Date <- as.Date(dt$Date, "%m/%d/%Y")

# Do analysis, no frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "No")

# Do analysis, with frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "Yes")

#myList <- makeEffPortAnalysis_TopN(dt,45,125, "Yes",  "No", c(5,15, 55))

# convert to data.table object
stacked.dt <- as.data.table(dt)
nSmooth <- 21
nExpRet <- 45
nExpCovar <- 125
# std premium = 0.5; 
#   21 smooth > 0.005
#    5 smooth > 0.020
#    3 smooth > 0.020
#    1 smooth > 0.070

riskPrem.max <- 0.005
RiskFreeRet.annual <- 0.020 # Set risk free annual return.  Will be converted to the appropriate units later
nPeriods <- 252 # number of periods for the native data to make 1 year - used to annualize the results from the input data.

lblBase <- paste(": (nRet=", nExpRet, "; nCovar=", nExpCovar, "; nSmooth=", nSmooth, "; RiskFree=", RiskFreeRet.annual, "; RiskPremMax=", riskPrem.max, ")", sep = "")

#Remove data for any Ticker where the number of data points is less than the max samples requested for either Expected Return or Expected Covariance.
maxN <- max(nExpRet, nExpCovar) + nSmooth + 5 # pad by 5 to allow for calculation of returns, and a little more
stacked.orig.dt <- stacked.dt
stacked.dt <- stacked.orig.dt[, nSamp := .N, , by=Ticker][(nSamp>=maxN)]
stacked.dropped.dt <- stacked.orig.dt[, nSamp := .N, , by=Ticker][(nSamp<maxN)]
#str(stacked.orig.dt)
#str(stacked.dt)
#str(stacked.dropped.dt)

# create indexed values, normalized by most recent date value
stacked.dt <- stacked.dt[order(Ticker, -Date)]
stacked.dt[, IdxPrice := Price/Price[1], by = Ticker]
stacked.dt[, SmthIdxPrice := rollmean(IdxPrice, nSmooth, align="center", fill=NA), by = Ticker]
# calculate arithmetic returns, multiplier, and log diff. NB:  use a 'lead' shift as  the data is currently sorted in descending chronological order.
stacked.dt[, Returns := SmthIdxPrice / shift(SmthIdxPrice, 1, type="lead") - 1, by = Ticker]
stacked.dt[, Multiplier := SmthIdxPrice / shift(SmthIdxPrice, 1, type="lead") , by = Ticker]
stacked.dt[, LogTen := log10(SmthIdxPrice) , by = Ticker]
stacked.dt[, LogDiff := log10(SmthIdxPrice) - log10(shift(SmthIdxPrice, 1, type="lead")) , by = Ticker]

# plot the normalized Price values (Relative Price)
p <- ggplot(stacked.dt, aes(x = Date, y = IdxPrice, color = Ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Relative Price (most recent = 1.0)") +
  xlab("Date") + ylab("Relative Price") +
  scale_color_discrete(name = "Asset")
print(p)

# plot the normalized Price values (Relative Price)
p <- ggplot(stacked.dt, aes(x = Date, y = SmthIdxPrice, color = Ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle(paste("Smoothed Relative Price (most recent = 1.0) - nSmooth: ", nSmooth, sep = "")) +
  xlab("Date") + ylab("Relative Price - Smoothed") +
  scale_color_discrete(name = "Asset")
print(p)


#drop any rows with NA that might have resulted from smoothing or other calculations
stacked.dt <- na.omit(stacked.dt)

#create all permutations of summaries
stacked.dt <- stacked.dt[order(Ticker, -Date)]
assets.summary.ret <- stacked.dt[, .(
  Exp.Ret = mean(.SD[1:nExpRet,Returns]),
  Std.Dev = sd(.SD[1:nExpCovar,Returns]),
  NSamples = .N
), by="Ticker"]
assets.summary.mult <- stacked.dt[, .(
  Exp.Ret = mean(.SD[1:nExpRet,Multiplier]),
  Std.Dev = sd(.SD[1:nExpCovar,Multiplier]),
  NSamples = .N
), by="Ticker"]
assets.summary.logdiff <- stacked.dt[, .(
  Exp.Ret = mean(.SD[1:nExpRet,LogDiff]),
  Std.Dev = sd(.SD[1:nExpCovar,LogDiff]),
  NSamples = .N
), by="Ticker"]

##################################################
# Covar for RETURNS
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.summary.new <- assets.summary.ret[order(-assets.summary.ret$Exp.Ret),]
crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'Returns')
crossTab.dt <- na.omit(crossTab.dt)
#str(crossTab.dt)

# sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
crossTab.dt <- crossTab.dt[order(-Date)]
#str(crossTab.dt)
#stop("really should stop now!!!")

crossTab.dt <- crossTab.dt[1:nExpCovar]
#str(crossTab.dt)
#stop("stopping now")

# calculate covariance of Asset columns, drop the Date column
assets.covar <- cov(crossTab.dt[,Date := NULL])
#str(assets.covar)

# Sort returns and covar in descending return order
#covar axis 1
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(Exp.Ret = assets.summary.ret$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(Exp.Ret = assets.summary.ret$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL

assets.covar.new.ret <- assets.covar.new
assets.summary.new.ret <- assets.summary.new
################################################

# Covar for Multiplier
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.expRet <- as.data.frame(assets.summary.mult$Exp.Ret)
assets.summary.new <- assets.summary.mult[order(-assets.summary.mult$Exp.Ret),]
crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'Multiplier')
crossTab.dt <- na.omit(crossTab.dt)

# sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
crossTab.dt <- crossTab.dt[order(-Date)]
crossTab.dt <- crossTab.dt[1:nExpCovar]

# calculate covariance of Asset columns, drop the Date column
assets.covar <- cov(crossTab.dt[,Date := NULL])

# Sort returns and covar in descending return order
#covar axis 1
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(Exp.Ret = assets.summary.mult$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(Exp.Ret = assets.summary.mult$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL

assets.covar.new.mult <- assets.covar.new
assets.summary.new.mult <- assets.summary.new
################################################

# Covar for LogDiff
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.expRet <- as.data.frame(assets.summary.logdiff$Exp.Ret)
assets.summary.new <- assets.summary.logdiff[order(-assets.summary.logdiff$Exp.Ret),]
crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'LogDiff')
crossTab.dt <- na.omit(crossTab.dt)

# sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
crossTab.dt <- crossTab.dt[order(-Date)]
crossTab.dt <- crossTab.dt[1:nExpCovar]

# calculate covariance of Asset columns, drop the Date column
assets.covar <- cov(crossTab.dt[,Date := NULL])

# Sort returns and covar in descending return order
#covar axis 1
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(Exp.Ret = assets.summary.logdiff$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(Exp.Ret = assets.summary.logdiff$Exp.Ret))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$Exp.Ret),]
assets.covar.new$Exp.Ret <- NULL

assets.covar.new.logdiff <- assets.covar.new
assets.summary.new.logdiff <- assets.summary.new
################################################
riskPrem.inc <- riskPrem.max/100


#Returns

myeff <- makeEfficientFrontier(assets.summary.new.ret$Exp.Ret, assets.covar.new.ret, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
retRiskFree.native <- EF_AnnualReturnToNative(RiskFreeRet.annual, "Returns", nPeriods)
myeff$Sharpe <- (myeff$Exp.Ret - retRiskFree.native) / myeff$Std.Dev

myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.ret <- myeff
myeff.ret.sorted <- EF_SortPortfolioCols(myeff, myeff.maxSharpe$Idx)

myeff.maxSharpe.ret <- myeff.maxSharpe
myeff.maxsharpe.ret.sorted <- EF_SortPortfolioCols(myeff.maxSharpe, 1)

##############
#Multiplier
myeff <- makeEfficientFrontier(assets.summary.new.mult$Exp.Ret, assets.covar.new.mult, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
retRiskFree.native <- EF_AnnualReturnToNative(RiskFreeRet.annual, "Multiplier", nPeriods)
myeff$Sharpe <- (myeff$Exp.Ret - retRiskFree.native) / myeff$Std.Dev

myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.mult <- myeff
myeff.mult.sorted <- EF_SortPortfolioCols(myeff, myeff.maxSharpe$Idx)

myeff.maxSharpe.mult <- myeff.maxSharpe
myeff.maxsharpe.mult.sorted <- EF_SortPortfolioCols(myeff.maxSharpe, 1)

##############
#LogDiff
myeff <- makeEfficientFrontier(assets.summary.new.logdiff$Exp.Ret, assets.covar.new.logdiff, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
retRiskFree.native <- EF_AnnualReturnToNative(RiskFreeRet.annual, "LogDiff", nPeriods)
myeff$Sharpe <- (myeff$Exp.Ret - retRiskFree.native) / myeff$Std.Dev

myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.logdiff <- myeff
myeff.logdiff.sorted <- EF_SortPortfolioCols(myeff, myeff.maxSharpe$Idx)

myeff.maxSharpe.logdiff <- myeff.maxSharpe
myeff.maxsharpe.logdiff.sorted <- EF_SortPortfolioCols(myeff.maxSharpe, 1)


############## EF Charts
## Returns
mylbl <- paste("-Returns", lblBase, sep = "")
EF_MakePlots(myeff.ret, myeff.maxSharpe.ret, assets.summary.ret, mylbl, RiskFreeRet.annual, "Returns", 252)

## Multiplier
mylbl <- paste("-Multiplier", lblBase, sep = "")
EF_MakePlots(myeff.mult, myeff.maxSharpe.mult, assets.summary.mult, mylbl, RiskFreeRet.annual, "Multiplier", 252)

## LogDiff
mylbl <- paste("-LogDiff", lblBase, sep = "")
EF_MakePlots(myeff.logdiff, myeff.maxSharpe.logdiff, assets.summary.logdiff, mylbl, RiskFreeRet.annual, "LogDiff", 252)



