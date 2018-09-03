require(data.table)
require(zoo)
require(ggplot2)

library(data.table)
library(zoo)
library(ggplot2)


source('C:/Users/ao872/Downloads/R_POC/scripts/EffPortSolveTLB.R')
source('C:/Users/ao872/Downloads/R_POC/scripts/EffPortAnalyzeTLB.R')

#Input Zimmerman data
#dt <- fread('C:/Users/Tom/OneDrive/OD_Documents/EfficientPortfolioStuff/EffPort_R_BarChart/R_POC/Data/fin_data.csv')
#dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/fin_data.csv')
#dt[, Date := as.Date(date)]
#dt[, Ticker := ticker]
#dt[, Price := price]

#Input Vanguard data
dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/VanguardETFStacked_2018_05.csv')
dt$Date <- as.Date(dt$Date, "%m/%d/%Y")

# Do analysis, no frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "No")

# Do analysis, with frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "Yes")

#myList <- makeEffPortAnalysis_TopN(dt,45,125, "Yes",  "No", c(5,15, 55))


myList <- EffFrontier.AnalyzeScenarios(dt,252,
                  listCalc = list("Return", "Multiplier", "LogDiff"), 
                  listScale = list("AnnualizedReturn", "Native"),
                  listRiskFree = list(0.05),
                  listNumSamples = list(list(nExpRet=45, nExpCovar=125)),
                  listShortFlag = list("No", "Yes"),
                  listTopN = list(5, 20, 55),
                  listPriceSmoothing = list(1, 3)
)

# convert to data.table object
stacked.dt <- as.data.table(dt)
nSmooth <- 3
nExpRet <- 45
nExpCovar <- 125

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
  theme_bw() + ggtitle("Smoothed Relative Price (most recent = 1.0)") +
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
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

p <- ggplot() +
  geom_point(data=assets.summary.ret, aes(x = ExpSD*sqrt(252), y = (((ExpRet+1)^252)-1), color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Return Annualized Ret of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
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
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

p <- ggplot() +
  geom_point(data=assets.summary.mult, aes(x = ExpSD*sqrt(252), y = (ExpRet^252)-1, color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-Multiplier Annualized Ret of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
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
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

p <- ggplot() +
  geom_point(data=assets.summary.logdiff, aes(x = (10^(ExpSD*sqrt(252))-1), y = ((10^(ExpRet*252))-1), color = Ticker), size = 5) +
  # Miscellaneous Formatting
  theme_bw() + 
  ggtitle(paste("Risk-logDiff Annualized of Individual Assets (nExpRet=", nExpRet, 
                "; nExpCovar=", nExpCovar, ")", sep = "") ) +
  xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
print(p)

