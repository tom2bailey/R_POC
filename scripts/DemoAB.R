require(data.table)
require(scales)
require(ggplot2)

library(data.table)
library(scales)
library(ggplot2)
#library(IDPmisc)


source('C:/Users/ao872/Downloads/R_POC/scripts/EffPortSolveTLB.R')
source('C:/Users/ao872/Downloads/R_POC/scripts/EffPortAnalyzeTLB.R')

#Input Zimmerman data
#dt <- fread('C:/Users/Tom/OneDrive/OD_Documents/EfficientPortfolioStuff/EffPort_R_BarChart/R_POC/Data/fin_data.csv')
dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/fin_data.csv')
dt[, Date := as.Date(date)]
dt[, Ticker := ticker]
dt[, Price := price]

#Input Vanguard data
#dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/VanguardETFStacked_2018_05.csv')
#dt$Date <- as.Date(dt$Date, "%m/%d/%Y")

# Do analysis (1 yr/5 year - 12, 60)
#myList <- makeEffPortAnalysis(dt,12,60, "Yes")

# Do analysis (10 yr/10 year - 120, 120)
#myList <- makeEffPortAnalysis(dt,120,120, "Yes")

# Do analysis Top N (1 yr/5 year - 12, 60)
myList <- makeEffPortAnalysis_TopN(dt,12,60, "Yes",  "No", c(2,3,4))
