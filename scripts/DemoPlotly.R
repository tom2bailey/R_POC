require(data.table)
require(scales)
require(ggplot2)
require(plotly)

library(data.table)
library(scales)
library(ggplot2)
library(plotly)

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

# convert to data.table object
stacked.dt <- as.data.table(dt)

# create indexed values, normalized by most recent date value
stacked.dt <- stacked.dt[order(Ticker, -Date)]
stacked.dt[, IdxPrice := Price/Price[1], by = Ticker]

p <- ggplot(stacked.dt, aes(x = Date, y = IdxPrice, color = Ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Relative Price (most recent = 1.0)") +
  xlab("Date") + ylab("Relative Price") +
  scale_color_discrete(name = "Asset")
pp <-ggplotly(p)
rangeslider(pp)

# Do analysis, no frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "No")

# Do analysis, with frontiers (~ 2mths/6mths - 45, 125)
#myList <- makeEffPortAnalysis(dt,45,125, "Yes")
