makeEffPortAnalysis_TopN <- function ( stackedTicker, nExpRet, nExpCovar, effFlag, shortFlag, vecTopN){
  # Input Args
  # stackedTicker - data frame / data table of m rows, must have columns which include
    # Date - Must be sortable in chronological order, and used for x-axis of charts.  
        #Otherwise not used, so can be any type of value.  Could be mixed date/time.
    # Ticker - symbol of the security or asset for this row
    # Price - price of the security at this point in time
  # nExpRet - number of points to include when calculating expected return of each asset
  # nExpCovar - number of points to include for calculating Std Dev of each asset, and covariances
  # effFlag - "Yes" means to do full analysis and calculate efficient frontiers; "No" means simple charts only.
  # shortFlag - "Yes" run frontier analysis with shorting, "No" - shorting not allowed
  # topN - vector of numbers - top return tickers to process in each case.  Must have at least one element in the vector
  
  
  # the data analysis creates:
    # IdxPrice - column of normalized asset prices, normalized to the most recent price
    # Returns - column of sample to sample returns

  library(data.table)
  library(scales)
  library(ggplot2)
 # library(IDPmisc)

  
  # convert to data.table object
  stacked.dt <- as.data.table(stackedTicker)
  
  # create indexed values, normalized by most recent date value
  stacked.dt <- stacked.dt[order(Ticker, -Date)]
  stacked.dt[, IdxPrice := Price/Price[1], by = Ticker]
 
  # calculate arithmetic returns. NB:  use a 'lead' shift as  the data is currently sorted in descending chronological order.
  stacked.dt[, Returns := Price / shift(Price, 1, type="lead") - 1, by = Ticker]
  # returns the same as the following when data is in *ascending* chrono order
  #stacked.dt[, Returns := Price / shift(Price, 1) - 1, by = Ticker]
  
  # create summary table for plotting individual asset data on Volatility - Return charts
  # stacked ticker data must be in descending chronological order to catch the appropriate data points.
  stacked.dt <- stacked.dt[order(Ticker, -Date)]
  assets.summary <- stacked.dt[, .(
    ExpRet = mean(.SD[1:nExpRet,Returns]),
    ExpSD = sd(.SD[1:nExpCovar,Returns]),
    NSamples = .N
    ), by="Ticker"]
  nAssets <- nrow(assets.summary)
  
  print("Asset Summary")
  print(assets.summary)
  
  # Add entry for Risk Free asset to visualize CAPM line
  # Assume zero return Risk-Free asset for now
  # !!!!Future: risk free asset units must match the type of analysis path we are using
  # !!!!Future: convert from annualized return to the appropriate units (daily return, daily multiplier, log diff)
  assets.sumplot <- rbind(assets.summary, data.frame(Ticker="Risk Free", ExpRet=0, ExpSD=0, NSamples=1))
  
  # create cross-tab of asset data so we can calculate covariances. clear any null values.
  crossTab.dt <- dcast(stacked.dt, Date~Ticker, value.var = 'Returns')
  crossTab.dt <- na.omit(crossTab.dt)

  # sort in descending chronological order. Trim to the number of rows requested for the covariance calculation
  crossTab.dt <- crossTab.dt[order(-Date)]
  crossTab.dt <- crossTab.dt[1:nExpCovar]
  
  # calculate covariance of Asset columns, drop the Date column
  assets.covar <- cov(crossTab.dt[,Date := NULL])
  
  # re-sort in ascending Chrono order
  stacked.dt <- stacked.dt[order(Ticker, Date)]
  #print(str(stacked.dt))
  
  # Sort returns and covar in descending return order
  #covar axis 1
  assets.covar.new <- cbind(as.data.frame(assets.covar), data.frame(ExpRet = assets.summary$ExpRet))
  assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
  assets.covar.new$ExpRet <- NULL
  #covar axis 2
  assets.covar.new <- as.data.frame(t(assets.covar.new))
  assets.covar.new <- cbind(as.data.frame(assets.covar.new), data.frame(ExpRet = assets.summary$ExpRet))
  assets.covar.new <- assets.covar.new[order(-assets.covar.new$ExpRet),]
  assets.covar.new$ExpRet <- NULL
  
  assets.summary.new <- assets.summary[order(-assets.summary$ExpRet),]
  
#  print(assets.summary)
#  print(assets.covar)
  
#  print(assets.summary.new)
#  print(assets.covar.new)
  
  
  #- plots ------------
  
  
  
  #---------
  #end of basic analysis
  # loop for vector of topN values
    # process eff frontier, max sharpe
    # trim max sharpe to only one row
    # save in list of lists???
  # post process for graphics, or add graphics flags
  
  effList <- vector("list",length(vecTopN))
  MxSharpePortList <- vector("list", length(vecTopN))
  MxSharpeStatList <- vector("list", length(vecTopN))
  print("length of vectopn")
  length(vecTopN)
  
  if (effFlag=="Yes") {
    iTopN <- 1
    for(i in vecTopN) {
    
      #ensure requested top N assets isn't more than we have. 
      if (i<nAssets) {iTop <- i} else {iTop <- nAssets}
      assets.summary.topN <- as.data.frame(assets.summary.new[1:iTop, ])
      assets.covar.topN <- as.data.frame(assets.covar.new[1:iTop,1:iTop])
        
      myeff <- makeEfficientFrontier(as.matrix(assets.summary.topN$ExpRet), as.matrix(assets.covar.topN), short="no",risk.premium.up = 0.2, risk.increment = 0.002)
      effList[[iTopN]] <- myeff
      
      myeff.maxSharpe <- myeff[myeff$Sharpe==max(myeff$Sharpe),]
      myeff.maxSharpe <- myeff.maxSharpe[1,] # trim to 1 row max, just in case two rows matched max Sharpe value
      tmp<-myeff.maxSharpe[,1:iTop]
      MxSharpePortList[[iTopN]] <- as.data.frame(t(tmp[,order(-tmp[1,])]))
      MxSharpeStatList[[iTopN]] <- myeff.maxSharpe[1,(iTop+1):(iTop+4)]
            
      print(paste("Max Sharpe: Top N= (",iTop, ")", sep=""))
      print(as.data.frame(t(tmp[,order(-tmp[1,])])))
      
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
      
      #Individual curve chart
      p <- ggplot() + 
        geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealred) +
        geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Return), color=ealred, fill=ealred, size=4, shape=24) +
        annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Return,
                 label=paste("TopN: ", iTop , sep=""), hjust=0, vjust=1.2) +
        geom_point(data=assets.sumplot, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
        ggtitle(paste("Efficient Frontier and Optimal Portfolio - TopN: ", iTop, sep="")) +
        labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
        theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
              plot.title=element_text( color=ealred))
      print(p)
      

      
      myeffmelt <- melt(myeff[,c(1:iTop, iTop+4)], id.vars = "Idx")
      p <- ggplot() +  
        geom_col(data=myeffmelt, aes(x=Idx,y=value,fill=variable)) +
        geom_point(data=myeff.maxSharpe, aes(x=Idx, y=0.5) , size=4) +
        ggtitle(paste("Portfolios - No Short (nExpRet=", nExpRet, 
                      "; nExpCovar=", nExpCovar, "; iTop=", iTop, ")", sep = "") ) +
        annotate(geom="text", x=myeff.maxSharpe$Idx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
      print(p)
      
      p <- ggplot() +
        geom_line(data=myeff, aes(x=Idx, y=Sharpe)) +
        geom_point(data=myeff.maxSharpe, aes(x=Idx, y=myeff.maxSharpe$Sharpe) , size=4) +
        ggtitle(paste("Sharpe vs Idx - No Short (nExpRet=", nExpRet, 
                      "; nExpCovar=", nExpCovar, "; iTop=", iTop, ")", sep = "") ) +
        annotate(geom="text", x=myeff.maxSharpe$Idx, y=myeff.maxSharpe$Sharpe, label="max Sharpe", hjust=-0.1, vjust = 0.5)
      print(p)
      
      iTopN <- iTopN + 1
  #end of if for effFlag="Yes" and for i in vecTopN
  }}
  
#  if (effFlag=="Yes") {
#    my.list <- list(
#      c("assets.summary"), as.data.frame(assets.sumplot),
#      c("assets.covar"), as.data.frame(assets.covar),
#      c("assets.heat"), as.data.frame(assets.heat),
#      c("ShortingAllowed.Eff.df"), as.data.frame(myeff),
#      c("ShortingAllowed.MaxSharpe.df"), as.data.frame(myeff.maxSharpe),
#      c("NoShorting.Eff.df"), as.data.frame(myeffns),
#      c("NoShorting.MaxSharpe.df"), as.data.frame(myeffns.maxSharpe)
#    )
#    
#  } else {
    my.list <- list(
      assets_summary = as.data.frame(assets.sumplot),
      assets_covar = as.data.frame(assets.covar),
      vecTopN = vecTopN,
      eff_List = effList,
      mx_SharpePortList = MxSharpePortList,
      mx_SharpeStatList = MxSharpeStatList
#      ,
#      c("assets.heat"), as.data.frame(assets.heat)
      )
    

#  }
  str(effList)
  str(MxSharpePortList)
  str(MxSharpeStatList)
  
    
  return( my.list )  
  
}