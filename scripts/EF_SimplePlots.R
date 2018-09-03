EF_geom_rect_Portfolios <- function( df.effResults ){
  # creates a ggplot2 geom_rect() to add to a plot.
  # the set of rectangles represents stacked bar charts (y-axis) of each weighted portfolio along the frontier,
  #   vs. the Std Dev of the portfolio (x-axis)
  
  # local copy, just to be safe
  myeffz <- df.effResults
  
  # create ymin and ymax for each bar, at each portfolio point
  # each row is a portfolio point
  # cumsum of each row is the major work for top or bottom of bars
  nTicks <- ncol(myeffz)-4
  qcs <-t(apply(myeffz[,1:nTicks], 1, cumsum))
  ymax <- qcs
  ymin <- cbind(integer(nrow(qcs)), qcs[,1:(ncol(qcs)-1)])

  # create xmin and xmax for each vertical set of bars.  One vertical set for each portfolio point
  # left-most bar uses the same width as the 2nd left most bar
  nSamp <- nrow(myeffz)
  xmin <- myeffz$Std.Dev
  xmax <- myeffz$Std.Dev[2:nSamp]
  xmax[nSamp] <- 2*xmin[nSamp] -xmin[nSamp-1]

  # melt (reshape) the ymin, ymax matrix results into long vectors, 
  # Var1 = row index
  # Var2 = column names, which are the ticker labels
  # value = the cumsum values
  # as of July 2018, this melt() command goes down all rows of the first column, 
  #   the moves to the second column, etc.
  # double-check this melt() behavior if you recode this or if there are issues.
  zymin <- melt(ymin)
  zymax <- melt(ymax)
  
  # make replicated xmin and xmax vectors, and assemble into the rectangle dataframe
  zxmin <- rep(t(xmin),nTicks)
  zxmax <- rep(t(xmax),nTicks)
  r.df <- data.frame(xmn = zxmin, xmx = zxmax, ymn = zymin$value, ymx = zymax$value, Ticker = zymax$Var2)

  # create ggplot with only the geom_rect element (layer?), with black frames around each rectangle
  p <- ggplot() +
    geom_rect(data = r.df, aes(xmin=xmn, xmax=xmx, ymin=ymn,ymax=ymx,fill=Ticker), color="black")

  return(p)
}

EF_CleanEFPorts <- function( df.effResults ) {
  # cleans up EF results:
  #   removes ticker columns that were never used in a portfolio on the frontier
  #   removed rows where the standard deviation is no longer increasing
  # structure of data frame:
  #   1..n columns of ticker weights
  #   n..(n+4) columns of other info (Exp.Ret, Std.Dev, Sharpe, Idx)
  
  #clean columns
  nTickers <- ncol(df.effResults)-4
  tickSums <- data.frame(t(colSums(df.effResults[,1:nTickers])))
  df.effclean <- df.effResults[,c(which(tickSums[1,]>0.0001), nTickers+(1:4) )]
  
  #clean rows
  maxStdDev <- max(df.effclean$Std.Dev)
  df.effclean$sDelta <- shift(df.effclean$Std.Dev,1,type = "lead") - df.effclean$Std.Dev
  df.effclean.short <- df.effclean[ which(df.effclean$sDelta>(maxStdDev/1000)),]
  df.effclean.short$sDelta <- NULL

  #sort Ticker columns in descending order of 'overall usage'
  nTickers <- ncol(df.effclean.short)-4
  tickSums <- data.frame(t(colSums(df.effclean.short[,1:nTickers])))
  df.effclean.short <- df.effclean.short[,c(order(-tickSums), nTickers+(1:4) )]
  
  return(df.effclean.short)
}
EF_SortPortfolioCols <- function( df.effResults, iRowToSort) {
  # sorts the ticker weight columns of the data frame (data table?) by descending portfolio weight.  Other columns remain as-is.
  # structure of data frame:
  #   1..n columns of ticker weights
  #   n..(n+4) columns of other info (Exp.Ret, Std.Dev, Sharpe, Idx)
  # sorts by values of selected row
  
  nPorts <- nrow(df.effResults)
  nAssets <- ncol(df.effResults)

  #check that requested sort value rows is valid
  if (iRowToSort>nPorts) {
    stop(
      paste("EF_SortPortfolioCols: ",
        "selected sort row is beyond the end of data. iRowToSort: ", iRowToSort, sep=""))
    }

  # get column sort order by descending value.
  colOrder <- order(-df.effResults[iRowToSort,1:(nAssets-4)])
  
  df.sorted <- df.effResults[,c(colOrder, (nAssets-3):nAssets)]
  return(df.sorted)
}
  

EF_AnnualReturnToNative <- function(retAnnual, typeReturn, nPeriodsToAnnual){
  # Converts Annual arithmetic return to a native return units of the secified type of return.
  # retAnnual: number - annual arithmetic return to convert
  # typeReturn: string value specifying which type of native returns to calculate
  #   "Returns" - arithmetic returns: [price(n)/price(n-1) - 1]
  #   "Multiplier" - arithmetic multiplier: [price(n)/price(n-1)]
  #   "LogDiff" - log10 difference: [log10(price(n) - log10(price(n-1)] or [log10(Multiplier)]
  # nPeriodsToAnnual - integer number of periods from native data to annualized returns
  #   Note: some conversion periods are "by convention", not by strict calendar calculation
  #   252 - converts from daily data to annualized (based on average number of trading days year and alignment with other periods)
  #    50 - converts from weekly data to annualized (approx)
  #    12 - converts from monthly data to annualized

  #make native version of risk free return
  if (typeReturn=="Returns") {
    retAnnual.native <- (retAnnual +1)^(1/nPeriods)-1
    
  } else if (typeReturn=="Multiplier") {
    retAnnual.native <- (retAnnual +1)^(1/nPeriods)
    
  } else if (typeReturn=="LogDiff") {
    retAnnual.native <- log10(retAnnual +1)/nPeriods
    
  } else {
    stop(paste("EF_AnnualReturnToNative - Invalid typeReturn value: ", typeReturn, sep=""))
  }

  return(retAnnual.native)  
  
}

EF_NativeToAnnualizedReturn <- function( dObj, typeReturn, nPeriodsToAnnual) {
  # Converts expected returns and standard deviations from native units to annualized arithmetic return.
  # dObj: a data frame or data table (or some object) that has named columns of Exp.Ret and Std.Dev - in native data units
  # typeReturn: string value specifying which type of native returns are being input
  #   "Returns" - arithmetic returns: [price(n)/price(n-1) - 1]
  #   "Multiplier" - arithmetic multiplier: [price(n)/price(n-1)]
  #   "LogDiff" - log10 difference: [log10(price(n) - log10(price(n-1)] or [log10(Multiplier)]
  # nPeriodsToAnnual - integer number of periods from native data to annualized returns
  #   Note: some conversion periods are "by convention", not by strict calendar calculation
  #   252 - converts from daily data to annualized (based on average number of trading days year and alignment with other periods)
  #    50 - converts from weekly data to annualized (approx)
  #    12 - converts from monthly data to annualized
  #   other periods of interest:
  #     5 days per week
  #    21 days per month
  #    63 days per quarter
  #
  # conversion formulas - example for daily to annual (returns *not* converted to percentages):
  #   Returns:    annualized_return =     (period_return + 1)^252 - 1
  #   Multiplier: annualized_return =     (period_multiplier)^252 - 1
  #   LogDiff:    annualized_return =    (10^(period_logdiff*252)) - 1
  #
  #   Returns:    annualized_stddev =       period_return_stddev * sqrt(252)
  #   Multiplier: annualized_stddev =   period_multiplier_stddev * sqrt(252)  (note: same as Returns)
  #   LogDiff:    annualized_stddev = (10^(period_logdiff_stddev * sqrt(252))) - 1
  # 
  # conversion references:
  #   https://www.fool.com/knowledge-center/how-to-convert-daily-returns-to-annual-returns.aspx
  #   https://www.fool.com/knowledge-center/how-to-calculate-annualized-volatility.aspx
  
  dObj.annual <- dObj

  if (typeReturn=="Returns") {
    dObj.annual$Exp.Ret <- (dObj$Exp.Ret +1)^nPeriodsToAnnual - 1
    dObj.annual$Std.Dev <-  dObj$Std.Dev*sqrt(nPeriodsToAnnual)

  } else if (typeReturn=="Multiplier") {
    dObj.annual$Exp.Ret <- (dObj$Exp.Ret)^nPeriodsToAnnual - 1
    dObj.annual$Std.Dev <-  dObj$Std.Dev * sqrt(nPeriodsToAnnual)

  } else if (typeReturn=="LogDiff") {
    dObj.annual$Exp.Ret <-  (10^(dObj$Exp.Ret *      nPeriodsToAnnual )) - 1
    dObj.annual$Std.Dev <-  (10^(dObj$Std.Dev * sqrt(nPeriodsToAnnual))) - 1

  } else {
    stop(paste("EF_NativeToAnnualized - Invalid typeReturn value: ", typeReturn, sep=""))
  }
  
  return(dObj.annual)
  }

EF_MakePlots <- function (myeff, myeff.maxSharpe, assets.summary, lbl, retRiskFree, typeReturn, nPeriods){
  # Lots of Plots
  # retRiskFree: annual risk free return for Sharpe ratio adjustments
  # typeReturn - string of "Returns", "Multiplier", "LogDiff" for type of return method used
  # nPeriods - integer number of periods to convert from native data to annualized
  
  #make annualized versions of input data so we can chart as we please.
  myeff.annual <- EF_NativeToAnnualizedReturn(myeff, typeReturn, nPeriods)
  myeff.maxSharpe.annual <- EF_NativeToAnnualizedReturn(myeff.maxSharpe, typeReturn, nPeriods)
  assets.summary.annual <- EF_NativeToAnnualizedReturn(assets.summary, typeReturn, nPeriods)
  
  
  nAssets <- nrow((assets.summary))

  retRiskFree.native <- EF_AnnualReturnToNative(retRiskFree, typeReturn, nPeriods)
  asset.riskfree.annual <- data.frame("Risk"=c(0), "Reward"=c(retRiskFree))
  asset.riskfree.native <- data.frame("Risk"=c(0), "Reward"=c(retRiskFree.native))

  
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
  
  
  # plot the individual assets: Volatility - Return chart
  p <- ggplot() +
    geom_point(data=assets.summary, aes(x = Std.Dev, y = Exp.Ret, color = Ticker), size = 5) +
    geom_point(data=asset.riskfree.native, aes(x = Risk, y = Reward, color = "Risk Free"), size = 5) +
    theme_bw() + 
    ggtitle(paste("Risk-Return Indiv Assets Native ", lbl, sep = "") ) +
    xlab("Risk (Std Deviation)") + ylab("Expected Returns") 
  print(p)
  
  # plot the individual assets Annualized: Volatility - Return chart
  p <- ggplot() +
    geom_point(data=assets.summary.annual, aes(x = Std.Dev, y = Exp.Ret, color = Ticker), size = 5) +
    geom_point(data=asset.riskfree.annual, aes(x = Risk, y = Reward, color = "Risk Free"), size = 5) +
    theme_bw() + 
    ggtitle(paste("Risk-Return Indiv Assets Annualized ", lbl, sep = "") ) +
    xlab("Risk (Std Deviation)") + ylab("Expected Returns") 
  print(p)

  #Individual curve chart - Native
  p <- ggplot() + 
    geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Ret), alpha=.1, color=ealred) +
    geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Ret), color=ealred, fill=ealred, size=4, shape=24) +
    annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Ret,
             label="maxSharpe: ", hjust=0, vjust=1.2) +
    geom_point(data=assets.summary, aes(x = Std.Dev, y = Exp.Ret, color = Ticker), size = 5) +
    geom_point(data=asset.riskfree.native, aes(x = Risk, y = Reward, color = "Risk Free"), size = 5) +
    ggtitle(paste("Eff Front -Native", lbl, sep = "")) +
    xlab("Risk (Std Deviation)") + ylab("Expected Returns") +
    theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
          plot.title=element_text( color=ealred))
  print(p)
  
  #Individual curve chart - Annualized
  p <- ggplot() + 
    geom_point(data=myeff.annual, aes(x=Std.Dev, y=Exp.Ret), alpha=.1, color=ealred) +
    geom_point(data=myeff.maxSharpe.annual, aes(x=Std.Dev, y=Exp.Ret), color=ealred, fill=ealred, size=4, shape=24) +
    annotate(geom="text", x=myeff.maxSharpe.annual$Std.Dev, y=myeff.maxSharpe.annual$Exp.Ret,
             label="maxSharpe: ", hjust=0, vjust=1.2) +
    geom_point(data=assets.summary.annual, aes(x = Std.Dev, y = Exp.Ret, color = Ticker), size = 5) +
    geom_point(data=asset.riskfree.annual, aes(x = Risk, y = Reward, color = "Risk Free"), size = 5) +
    ggtitle(paste("Eff Front -Annualized", lbl, sep = "")) +
    xlab("Risk (Std Deviation)") + ylab("Expected Returns") +
    theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
          plot.title=element_text( color=ealred))
  print(p)
  
  # Portfolio stacked columns
  # Portfolio stacked columns vs. IDX
  myeff.clean <- EF_CleanEFPorts( myeff)
  nClean <- ncol(myeff.clean) -4
  myeff.clean.annual <- EF_CleanEFPorts( myeff.annual)
  nClean.annual <- ncol(myeff.clean.annual) -4
  
  myeff.z <- myeff
  myeff.z.maxSharpe <- myeff.maxSharpe
  lbl2 <- "Portfolios vs. Idx Full"
  nTicks <- ncol(myeff.z) -4
  myeffmelt <- melt(myeff.z[, c(1:nTicks, nTicks+4)], id.vars = "Idx")
  p <- ggplot() +  
    geom_col(data=myeffmelt, aes(x=Idx,y=value,fill=variable)) +
    geom_point(data=myeff.z.maxSharpe, aes(x=Idx, y=0.5) , size=4) +
    ggtitle(paste(lbl2, lbl, sep = "" )) +
    annotate(geom="text", x=myeff.z.maxSharpe$Idx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
  print(p)
  
  # Portfolio stacked columns vs. IDX - cleaned Ticker columns
  myeff.z <- EF_CleanEFPorts( myeff)
  myeff.z.maxSharpe <- myeff.maxSharpe
  lbl2 <- "Portfolios vs. Idx Clean"
  nTicks <- ncol(myeff.z) -4
  myeffmelt <- melt(myeff.z[, c(1:nTicks, nTicks+4)], id.vars = "Idx")
  p <- ggplot() +  
    geom_col(data=myeffmelt, aes(x=Idx,y=value,fill=variable)) +
    geom_point(data=myeff.z.maxSharpe, aes(x=Idx, y=0.5) , size=4) +
    ggtitle(paste(lbl2, lbl, sep = "" )) +
    annotate(geom="text", x=myeff.z.maxSharpe$Idx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
  print(p)
  
  # Portfolio stacked columns vs. Annualized Std Dev - cleaned Ticker columns
  # Note:  can't use geom_col, must use custom EF_geom_rect_Portfolios()
  # Note:  use ..maxSharpe$Std.Dev, not ..$Idx for this chart
  myeff.z <- EF_CleanEFPorts( myeff.annual)
  myeff.z.maxSharpe <- myeff.maxSharpe.annual
  lbl2 <- "Portfolios vs. Annual StdDev Clean"

    p <- EF_geom_rect_Portfolios( myeff.z) + 
    geom_point(data=myeff.z.maxSharpe, aes(x=Std.Dev, y=0.5) , size=4) +
    ggtitle(paste(lbl2, lbl, sep = "" )) +
    annotate(geom="text", x=myeff.z.maxSharpe$Std.Dev, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
  print(p)
  
  # Sharpe Ratio vs. IDX
  p <- ggplot() +
    geom_line(data=myeff, aes(x=Idx, y=Sharpe)) +
    geom_point(data=myeff.maxSharpe, aes(x=Idx, y=myeff.maxSharpe$Sharpe) , size=4) +
    ggtitle(paste("Sharpe vs. Idx", lbl, sep = "" ) ) +
    annotate(geom="text", x=myeff.maxSharpe$Idx, y=myeff.maxSharpe$Sharpe, label="max Sharpe", hjust=-0.1, vjust = 0.5) +
    xlab("Idx") + ylab("Sharpe Ratio") 
  print(p)
  
  # Sharpe Ratio vs. Risk - Native
  p <- ggplot() +
    geom_line(data=myeff, aes(x=Std.Dev, y=Sharpe)) +
    geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=myeff.maxSharpe$Sharpe) , size=4) +
    ggtitle(paste("Sharpe vs. Risk Native", lbl, sep = "") ) +
    xlab("Risk (Std Deviation)") + ylab("Sharpe Ratio") +
    annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Sharpe, label="max Sharpe", hjust=-0.1, vjust = 0.5)
  print(p)
    
  # Sharpe Ratio vs. Risk - Annualized
  p <- ggplot() +
    geom_line(data=myeff.annual, aes(x=Std.Dev, y=Sharpe)) +
    geom_point(data=myeff.maxSharpe.annual, aes(x=Std.Dev, y=Sharpe) , size=4) +
    ggtitle(paste("Sharpe vs. Risk Annualized", lbl, sep = "") ) +
    xlab("Risk (Std Deviation)") + ylab("Sharpe Ratio") +
    annotate(geom="text", x=myeff.maxSharpe.annual$Std.Dev, y=myeff.maxSharpe.annual$Sharpe, label="max Sharpe", hjust=-0.1, vjust = 0.5)
  print(p)
    
}