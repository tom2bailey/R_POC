# take care of situation when one ticker has less than nExpRet or nExpCovar data points. (remove that ticker?)
# calculate the eff's, and then convert to annualized, then plot

# wrap script lines in '{}'s so stop will have the desired effect when running interactively in Rstudio
{
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

#Remove data for any Ticker where the number of data points is less than the max samples requested for either Expected Return or Expected Covariance.
maxN <- max(nExpRet, nExpCovar) + nSmooth + 5 # pad by 5 to allow for calculation of returns, and a little more
stacked.orig.dt <- stacked.dt
stacked.dt <- stacked.orig.dt[, nSamp := .N, , by=Ticker][(nSamp>=maxN)]
stacked.dropped.dt <- stacked.orig.dt[, nSamp := .N, , by=Ticker][(nSamp<maxN)]
str(stacked.orig.dt)
str(stacked.dt)
str(stacked.dropped.dt)

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
  ExpRet = mean(.SD[1:nExpRet,Returns]),
  ExpSD = sd(.SD[1:nExpCovar,Returns]),
  NSamples = .N
), by="Ticker"]
assets.summary.mult <- stacked.dt[, .(
  ExpRet = mean(.SD[1:nExpRet,Multiplier]),
  ExpSD = sd(.SD[1:nExpCovar,Multiplier]),
  NSamples = .N
), by="Ticker"]
assets.summary.logdiff <- stacked.dt[, .(
  ExpRet = mean(.SD[1:nExpRet,LogDiff]),
  ExpSD = sd(.SD[1:nExpCovar,LogDiff]),
  NSamples = .N
), by="Ticker"]

# plot the individual assets: Volatility - Return chart
p <- ggplot() +
  geom_point(data=assets.summary.ret, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Return of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

p <- ggplot() +
  geom_point(data=assets.summary.ret, aes(x = ExpSD*sqrt(252), y = (((ExpRet+1)^252)-1), color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Return Annualized Ret of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

#Multiplier
p <- ggplot() +
  geom_point(data=assets.summary.mult, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Multiplier of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)


p <- ggplot() +
  geom_point(data=assets.summary.mult, aes(x = ExpSD*sqrt(252), y = (ExpRet^252)-1, color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Multiplier Annualized Ret of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

#Log Diff method
p <- ggplot() +
  geom_point(data=assets.summary.logdiff, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-logDiff of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

p <- ggplot() +
  geom_point(data=assets.summary.logdiff, aes(x = (10^(ExpSD*sqrt(252))-1), y = ((10^(ExpRet*252))-1), color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-logDiff Annualized of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

##################################################
# Covar for RETURNS
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.summary.new <- assets.summary.ret[order(-assets.summary.ret$ExpRet),]
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
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(ExpRet = assets.summary.ret$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(ExpRet = assets.summary.ret$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL

assets.covar.new.ret <- assets.covar.new
assets.summary.new.ret <- assets.summary.new
################################################

# Covar for Multiplier
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.expRet <- as.data.frame(assets.summary.mult$ExpRet)
assets.summary.new <- assets.summary.mult[order(-assets.summary.mult$ExpRet),]
crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'Multiplier')
crossTab.dt <- na.omit(crossTab.dt)

# sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
crossTab.dt <- crossTab.dt[order(-Date)]
crossTab.dt <- crossTab.dt[1:nExpCovar]

# calculate covariance of Asset columns, drop the Date column
assets.covar <- cov(crossTab.dt[,Date := NULL])

# Sort returns and covar in descending return order
#covar axis 1
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(ExpRet = assets.summary.mult$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(ExpRet = assets.summary.mult$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL

assets.covar.new.mult <- assets.covar.new
assets.summary.new.mult <- assets.summary.new
################################################

# Covar for LogDiff
#create cross-tab of asset data so we can calculate covariances. clear any null values.
assets.expRet <- as.data.frame(assets.summary.logdiff$ExpRet)
assets.summary.new <- assets.summary.logdiff[order(-assets.summary.logdiff$ExpRet),]
crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'LogDiff')
crossTab.dt <- na.omit(crossTab.dt)

# sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
crossTab.dt <- crossTab.dt[order(-Date)]
crossTab.dt <- crossTab.dt[1:nExpCovar]

# calculate covariance of Asset columns, drop the Date column
assets.covar <- cov(crossTab.dt[,Date := NULL])

# Sort returns and covar in descending return order
#covar axis 1
assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(ExpRet = assets.summary.logdiff$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL
#covar axis 2
assets.covar.new <- as.data.frame(t(assets.covar.new))
assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(ExpRet = assets.summary.logdiff$ExpRet))
assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
assets.covar.new$ExpRet <- NULL

assets.covar.new.logdiff <- assets.covar.new
assets.summary.new.logdiff <- assets.summary.new
################################################
riskPrem.max <- 0.05
riskPrem.inc <- riskPrem.max/100

myeff <- makeEfficientFrontier(assets.summary.new.ret$ExpRet, assets.covar.new.ret, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
myeff$PortIdx <- seq.int(nrow(myeff))
myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.ret <- myeff
myeff.maxSharpe.ret <- myeff.maxSharpe

##############
myeff <- makeEfficientFrontier(assets.summary.new.mult$ExpRet, assets.covar.new.mult, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
myeff$PortIdx <- seq.int(nrow(myeff))
myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.mult <- myeff
myeff.maxSharpe.mult <- myeff.maxSharpe

##############
myeff <- makeEfficientFrontier(assets.summary.new.logdiff$ExpRet, assets.covar.new.logdiff, short="no",risk.premium.up = riskPrem.max, risk.increment = riskPrem.inc)
myeff$PortIdx <- seq.int(nrow(myeff))
myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]

myeff.logdiff <- myeff
myeff.maxSharpe.logdiff <- myeff.maxSharpe


# Color Scheme
ealred  <- "#7D110C"
ealtan  <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark  <- "#423C30"
# palattes from: http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
# black, light orange, light blue, green, yellow, dark blue, dark orange, purple 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

############## Native EF Charts
## Returns
mylbl <- paste("-Returns Native: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
EF_MakePlots(myeff.ret, myeff.maxSharpe.ret, assets.summary.ret, mylbl)

## Multiplier
mylbl <- paste("-Multiplier Native: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
EF_MakePlots(myeff.mult, myeff.maxSharpe.mult, assets.summary.mult, mylbl)

## LogDiff
mylbl <- paste("-LogDiff Native: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
EF_MakePlots(myeff.logdiff, myeff.maxSharpe.logdiff, assets.summary.logdiff, mylbl)

############## Annualized EF Charts
# Returns:    x = ExpSD*sqrt(252),          y = (((ExpRet+1)^252)-1)
# Multiplier: x = ExpSD*sqrt(252),          y = (ExpRet^252)-1,
# LogDiff:    x = (10^(ExpSD*sqrt(252))-1), y = ((10^(ExpRet*252))-1),
## Returns
mylbl <- paste("-Returns Annualized: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
myeff.annual <- myeff.ret
myeff.maxSharpe.annual <- myeff.maxSharpe.ret
myAsset.summary.annual <- assets.summary.ret
myeff.annual$Exp.Return <- (((myeff.annual$Exp.Return+1)^252)-1)
myeff.annual$Std.Dev <-  myeff.annual$Std.Dev * sqrt(252) 
myeff.maxSharpe.annual$Exp.Return <- (((myeff.maxSharpe.annual$Exp.Return+1)^252)-1)
myeff.maxSharpe.annual$Std.Dev <-  myeff.maxSharpe.annual$Std.Dev * sqrt(252) 
myAsset.summary.annual$ExpRet <-  (((myAsset.summary.annual$ExpRet+1)^252)-1)
myAsset.summary.annual$ExpSD <-  myAsset.summary.annual$ExpSD * sqrt(252) 

EF_MakePlots(myeff.annual, myeff.maxSharpe.annual, myAsset.summary.annual, mylbl)

## Multiplier
mylbl <- paste("-Multiplier Annualized: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
myeff.annual <- myeff.mult
myeff.maxSharpe.annual <- myeff.maxSharpe.mult
myAsset.summary.annual <- assets.summary.mult
myeff.annual$Exp.Return <- (((myeff.annual$Exp.Return)^252)-1)
myeff.annual$Std.Dev <-  myeff.annual$Std.Dev * sqrt(252) 
myeff.maxSharpe.annual$Exp.Return <- (((myeff.maxSharpe.annual$Exp.Return)^252)-1)
myeff.maxSharpe.annual$Std.Dev <-  myeff.maxSharpe.annual$Std.Dev * sqrt(252) 
myAsset.summary.annual$ExpRet <-  (((myAsset.summary.annual$ExpRet)^252)-1)
myAsset.summary.annual$ExpSD <-  myAsset.summary.annual$ExpSD * sqrt(252) 

EF_MakePlots(myeff.annual, myeff.maxSharpe.annual, myAsset.summary.annual, mylbl)

## LogDiff
mylbl <- paste("-LogDiff Annualized: (nExpRet=", nExpRet, "; nExpCovar=", nExpCovar, "; nSmooth=", nSmooth, ")", sep = "")
myeff.annual <- myeff.logdiff
myeff.maxSharpe.annual <- myeff.maxSharpe.logdiff
myAsset.summary.annual <- assets.summary.logdiff
myeff.annual$Exp.Return <-           ((10^(myeff.annual$Exp.Return          *252))-1)
myeff.annual$Std.Dev <-               (10^(myeff.annual$Std.Dev             *sqrt(252))-1)
myeff.maxSharpe.annual$Exp.Return <- ((10^(myeff.maxSharpe.annual$Exp.Return*252))-1)
myeff.maxSharpe.annual$Std.Dev <-     (10^(myeff.maxSharpe.annual$Std.Dev   *sqrt(252))-1)
myAsset.summary.annual$ExpRet <-     ((10^(myAsset.summary.annual$ExpRet    *252))-1)
myAsset.summary.annual$ExpSD <-       (10^(myAsset.summary.annual$ExpSD     *sqrt(252))-1)

EF_MakePlots(myeff.annual, myeff.maxSharpe.annual, myAsset.summary.annual, mylbl)


#geom_point(data=assets.summary.logdiff, aes(x = (10^(ExpSD*sqrt(252))-1), y = ((10^(ExpRet*252))-1), color = Ticker), size = 5) +

# end of wrap in '{}'s
}  