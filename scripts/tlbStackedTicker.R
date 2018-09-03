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
#dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/fin_data.csv')
#dt[, Date := as.Date(date)]
#dt[, Ticker := ticker]
#dt[, Price := price]

#Input Vanguard data
dt <- fread('C:/Users/ao872/Downloads/R_POC/Data/VanguardETFStacked_2018_05.csv')
dt$Date <- as.Date(dt$Date, "%m/%d/%Y")

# Do analysis
myList <- makeEffPortAnalysis(dt,45,135, "Yes")

# create indexed values
dt[, idx_price := price/price[1], by = ticker]

# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]
tabsum <- tab[, .(er = round(mean(ret), 4),
                  sd = round(sd(ret), 4)),
              by = "ticker"]

tabx <- dt[!is.na(ret), .(date, ticker, ret)]
tabx <- tabx[order(ticker,-date)]
#WRONG!!!
tabxsum <- tab[1:100, .(er = round(mean(ret), 4),
                  sd = round(sd(ret), 4)),
              by = "ticker"]

# summary cross-tab table, take only non-NA values
tabCrossRet.dt <- dcast(dt, date~ticker, value.var = 'ret')

#try na.omit instead.  It keeps the date column.
#tabCRetNoNull.dt <- NaRV.omit(tabCrossRet.dt) 
tabCRetNoNull.dt <- na.omit(tabCrossRet.dt) 

# summary, no nulls, drop date column
tabCRetNoNullNoDate <- tabCRetNoNull.dt
tabCRetNoNullNoDate[,date:=NULL] 

#future:  try sorting stacked tickers by ticker and date in reverse date
# then get means and sd by last (now first) xx rows
# then dcast to wide, and truncate to xx rows
# then sort back to ascending date order

#xx get this from Stacked version, but with the correct number of samples
# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
# tabsum <- tab[, .(er = round(mean(ret), 4),
#                   sd = round(sd(ret), 4)),
#               by = "ticker"]
#xx or consider reverse sort and take the (now) first xx rows for mean/expected ret, sd, and cov
#mean returns
tab.means <- tabCRetNoNullNoDate[,lapply(.SD, mean),]

#SD returns
tab.sd <- tabCRetNoNullNoDate[,lapply(.SD, sd),]

#CoVar returns
#must get this from unStacked version
tab.cov <- cov(tabCRetNoNullNoDate)

tab.plot <- data.frame(  ticker = colnames(tab.means), er = t(tab.means), sd = t(tab.sd))
ggplot(tab.plot, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))

myeff <- makeEfficientFrontier(tab.plot[,2],tab.cov, short="yes",risk.premium.up = 0.5, risk.increment = 0.005)
myeff$PortIdx <- seq.int(nrow(myeff))
myeff.maxSharpe <- myeff[myeff$sharpe==max(myeff$sharpe),]

myeffns <- makeEfficientFrontier(tab.plot[,2],tab.cov, short="no",risk.premium.up = 0.5, risk.increment = 0.005)
myeffns$PortIdx <- seq.int(nrow(myeffns))
myeffns.maxSharpe <- myeff[myeffns$sharpe==max(myeffns$sharpe),]

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

#Shorting allowed
ggplot() + 
  geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealdark) +
  geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Return, label=sharpe), color=ealred, size=5) +
  annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Return,
           label=paste("Shorting Risk: ", round(myeff.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                       round(myeff.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(myeff.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=-1.2) +
  geom_point(data=tab.plot, aes(x = sd, y = er, color = ticker), size = 5) +
  ggtitle("Efficient Frontier and Optimal Portfolio \n- Shorting") +
  labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))


#Shorting NOT allowed
ggplot() + 
  geom_point(data=myeffns, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealred) +
  geom_point(data=myeffns.maxSharpe, aes(x=Std.Dev, y=Exp.Return, label=sharpe), color=ealtan, size=5) +
  annotate(geom="text", x=myeffns.maxSharpe$Std.Dev, y=myeffns.maxSharpe$Exp.Return,
           label=paste("NoShort Risk: ", round(myeffns.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                       round(myeffns.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(myeffns.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=1.2) +
  geom_point(data=tab.plot, aes(x = sd, y = er, color = ticker), size = 5) +
  ggtitle("Efficient Frontier and Optimal Portfolio \n- No-Shorting") +
  labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))


#Both frontiers
ggplot() + 
  geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealdark) +
  geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Return, label=sharpe), color=ealred, size=5) +
  annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Return,
           label=paste("Shorting Risk: ", round(myeff.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                       round(myeff.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(myeff.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=-1.2) +
  geom_point(data=myeffns, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealred) +
  geom_point(data=myeffns.maxSharpe, aes(x=Std.Dev, y=Exp.Return, label=sharpe), color=ealtan, size=5) +
  annotate(geom="text", x=myeffns.maxSharpe$Std.Dev, y=myeffns.maxSharpe$Exp.Return,
           label=paste("NoShort Risk: ", round(myeffns.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                       round(myeffns.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(myeffns.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=1.2) +
  geom_point(data=tab.plot, aes(x = sd, y = er, color = ticker), size = 5) +
  ggtitle("Efficient Frontier and Optimal Portfolio \n- Shorting and No-Shorting") +
  labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))




myeffmelt <- melt(myeff[,c(1:3,7)], id.vars = "PortIdx")
ggplot() +  
  geom_col(data=myeffmelt, aes(x=PortIdx,y=value,fill=variable)) +
  geom_point(data=myeff.maxSharpe, aes(x=PortIdx, y=0.5) , size=4) +
  annotate(geom="text", x=myeff.maxSharpe$PortIdx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)


myeffnsmelt <- melt(myeffns[,c(1:3,7)], id.vars = "PortIdx")
ggplot() + 
  geom_col(data=myeffnsmelt, aes(x=PortIdx,y=value,fill=variable))+
  geom_point(data=myeffns.maxSharpe, aes(x=PortIdx, y=0.5) , size=4) +
  annotate(geom="text", x=myeffns.maxSharpe$PortIdx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
