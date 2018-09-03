# from: https://www.r-bloggers.com/a-gentle-introduction-to-finance-using-r-efficient-frontier-and-capm-part-1/
# Zimmerman
require(data.table)
require(scales)
require(ggplot2)
library(data.table)
library(scales)
library(ggplot2)

#link doesn't work for some reason.  File copied to fin_data.csv
#link <- "<a class="vglnk" href="https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/fin_data.csv" rel="nofollow"><span>https</span><span>://</span><span>raw</span><span>.</span><span>githubusercontent</span><span>.</span><span>com</span><span>/</span><span>DavZim</span><span>/</span><span>Efficient</span><span>_</span><span>Frontier</span><span>/</span><span>master</span><span>/</span><span>data</span><span>/</span><span>fin</span><span>_</span><span>data</span><span>.</span><span>csv</span></a>"
#dt <- data.table(read.csv(link))

dt <- fread('C:/Users/Tom/OneDrive/OD_Documents/EfficientPortfolioStuff/EffPort_R_BarChart/R_POC/Data/fin_data.csv')
dt[, date := as.Date(date)]

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

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tabsum <- tab[, .(er = round(mean(ret), 4),
                  sd = round(sd(ret), 4)),
              by = "ticker"]

ggplot(tabsum, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))