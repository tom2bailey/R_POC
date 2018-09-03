makeEffPortAnalysis <- function ( stackedTicker, nExpRet, nExpCovar, effFlag ){
  # Input Args
  # stackedTicker - data frame / data table of m rows, must have columns which include
    # Date - Must be sortable in chronological order, and used for x-axis of charts.  
        #Otherwise not used, so can be any type of value.  Could be mixed date/time.
    # Ticker - symbol of the security or asset for this row
    # Price - price of the security at this point in time
  # nExpRet - number of points to include when calculating expected return of each asset
  # nExpCovar - number of points to include for calculating Std Dev of each asset, and covariances
  # effFlag - "Yes" means to do full analysis and calculate efficient frontiers; "No" means simple charts only.
  
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
  # stacked ticker data must be in descending chronological order
  stacked.dt <- stacked.dt[order(Ticker, -Date)]
  assets.summary <- stacked.dt[, .(
    ExpRet = mean(.SD[1:nExpRet,Returns]),
    ExpSD = sd(.SD[1:nExpCovar,Returns]),
    NSamples = .N
    ), by="Ticker"]
  assets.sumplot <- rbind(assets.summary, data.frame(Ticker="Risk Free", ExpRet=0, ExpSD=0, NSamples=1))
  nAssets <- nrow(assets.summary)
  
  print("Asset Summary")
  print(assets.summary)
  
  # Add entry for Risk Free asset to visualize CAPM line
  # Assume zero return Risk-Free asset for now
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
  
  #-------------
  # plot the normalized Price values (Relative Price)
  p <- ggplot(stacked.dt, aes(x = Date, y = IdxPrice, color = Ticker)) +
    geom_line() +
    # Miscellaneous Formatting
    theme_bw() + ggtitle("Relative Price (most recent = 1.0)") +
    xlab("Date") + ylab("Relative Price") +
    scale_color_discrete(name = "Asset")
  print(p)
  
  # plot the individual assets: Volatility - Return chart
  p <- ggplot() +
    geom_point(data=assets.sumplot, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
    # Miscellaneous Formatting
    theme_bw() + 
    ggtitle(paste("Risk-Return of Individual Assets (nExpRet=", nExpRet, 
                  "; nExpCovar=", nExpCovar, ")", sep = "") ) +
    xlab("Volatility") + ylab("Expected Returns") 
#    scale_y_continuous(label = percent, limits = c(0, 0.03)) +
#    scale_x_continuous(label = percent, limits = c(0, 0.1))
  print(p)
  

  # heatmap of return vs. correlation
  # sort one axis vs. expected return; clean up sort column at the end
  assets.correl <- cbind(as.data.frame(cov2cor(assets.covar)), data.frame(ExpRet = assets.summary$ExpRet))
  assets.correl <- assets.correl[order(-assets.correl$ExpRet),]
  assets.correl$ExpRet <- NULL
  # sort the other axis by least correlation with the highest return asset
  assets.correl <- as.data.frame(t(assets.correl))
  assets.correl <- assets.correl[order(assets.correl[[1]]),]
  # make the heatmap
  assets.heat <- melt(as.matrix(assets.correl))
  p <- ggplot() +
    geom_tile(data=assets.heat, aes(x=Var1,y=Var2,fill = value)) +
    scale_fill_gradient2(low="green", high="red", #colors in the scale
                         breaks=seq(-1,1,0.25), #breaks in the scale bar
                         limits=c(-1, 1) ) + #same limits for plots
    labs(x="Lowest Correl -> to -> Highest", y="Highest ExpReturn -> to -> Lowest") +
    ggtitle( paste("Correlation - sorted vs. Highest Return Asset (nExpRet=", nExpRet, 
                  "; nExpCovar=", nExpCovar, ")", sep = "") )
  print(p)
  
  
  #---------
  #end of basic analysis
  
  if (effFlag=="Yes") {
    
    myeff <- makeEfficientFrontier(assets.summary$ExpRet, assets.covar, short="yes",risk.premium.up = 0.5, risk.increment = 0.005)
    myeff$PortIdx <- seq.int(nrow(myeff))
    myeff.maxSharpe <- myeff[myeff$sharpe==max(myeff$sharpe),]
    print("Max Sharpe: Shorting Allowed")
    print(as.data.frame(t(myeff.maxSharpe)))
    
    myeffns <- makeEfficientFrontier(assets.summary$ExpRet, assets.covar, short="no",risk.premium.up = 0.5, risk.increment = 0.005)
    myeffns$PortIdx <- seq.int(nrow(myeffns))
    myeffns.maxSharpe <- myeffns[myeffns$sharpe==max(myeffns$sharpe),]
    print("Max Sharpe: No Shorting")
    print(as.data.frame(t(myeffns.maxSharpe)))
    
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
    p <- ggplot() + 
      geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealdark) +
      geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Return), color=ealdark, fill=ealdark, size=4, shape=25) +
      annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Return,
               label=paste("Shorting Risk: ", round(myeff.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                           round(myeff.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                           round(myeff.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=-1.2) +
      geom_point(data=assets.sumplot, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
      ggtitle("Efficient Frontier and Optimal Portfolio - Shorting") +
      labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
      theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
            plot.title=element_text( color=ealred))
    print(p)
    
    #Shorting NOT allowed
    p <- ggplot() + 
      geom_point(data=myeffns, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealred) +
      geom_point(data=myeffns.maxSharpe, aes(x=Std.Dev, y=Exp.Return), color=ealred, fill=ealred, size=4, shape=24) +
      annotate(geom="text", x=myeffns.maxSharpe$Std.Dev, y=myeffns.maxSharpe$Exp.Return,
               label=paste("NoShort Risk: ", round(myeffns.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                           round(myeffns.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                           round(myeffns.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=1.2) +
      geom_point(data=assets.sumplot, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
      ggtitle("Efficient Frontier and Optimal Portfolio - No-Shorting") +
      labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
      theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
            plot.title=element_text( color=ealred))
    print(p)
    
    #Both frontiers
    p <- ggplot() + 
      geom_point(data=myeff, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealdark) +
      geom_point(data=myeff.maxSharpe, aes(x=Std.Dev, y=Exp.Return), color=ealdark, fill=ealdark, size=4, shape=25) +
      annotate(geom="text", x=myeff.maxSharpe$Std.Dev, y=myeff.maxSharpe$Exp.Return,
               label=paste("Shorting Risk: ", round(myeff.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                           round(myeff.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                           round(myeff.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=1, vjust=-0.5) +
      geom_point(data=myeffns, aes(x=Std.Dev, y=Exp.Return), alpha=.1, color=ealred) +
      geom_point(data=myeffns.maxSharpe, aes(x=Std.Dev, y=Exp.Return), color=ealred, fill=ealred, size=4, shape=24) +
      annotate(geom="text", x=myeffns.maxSharpe$Std.Dev, y=myeffns.maxSharpe$Exp.Return,
               label=paste("NoShort Risk: ", round(myeffns.maxSharpe$Std.Dev*100, digits=3),"\nReturn: ",
                           round(myeffns.maxSharpe$Exp.Return*100, digits=4),"%\nSharpe: ",
                           round(myeffns.maxSharpe$sharpe*100, digits=2), "%", sep=""), hjust=0, vjust=1.2) +
      geom_point(data=assets.sumplot, aes(x = ExpSD, y = ExpRet, color = Ticker), size = 5) +
      ggtitle("Efficient Frontier and Optimal Portfolio - Shorting and No-Shorting") +
      labs(x="Risk (standard deviation of portfolio variance)", y="Return") +
      theme(panel.background=element_rect(fill=eallighttan), text=element_text(color=ealdark),
            plot.title=element_text( color=ealred))
    print(p)
    
    
    
    myeffmelt <- melt(myeff[,c(1:nAssets, nAssets+4)], id.vars = "PortIdx")
    p <- ggplot() +  
      geom_col(data=myeffmelt, aes(x=PortIdx,y=value,fill=variable)) +
      geom_point(data=myeff.maxSharpe, aes(x=PortIdx, y=0.5) , size=4) +
      ggtitle(paste("Portfolios - Shorting Allowed (nExpRet=", nExpRet, 
                    "; nExpCovar=", nExpCovar, ")", sep = "") ) +
      annotate(geom="text", x=myeff.maxSharpe$PortIdx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
    print(p)
    
    myeffnsmelt <- melt(myeffns[,c(1:nAssets, nAssets+4)], id.vars = "PortIdx")
    p <- ggplot() + 
      geom_col(data=myeffnsmelt, aes(x=PortIdx,y=value,fill=variable))+
      ggtitle(paste("Portfolios - No Shorting (nExpRet=", nExpRet, 
                    "; nExpCovar=", nExpCovar, ")", sep = "") ) +
      geom_point(data=myeffns.maxSharpe, aes(x=PortIdx, y=0.5) , size=4) +
      annotate(geom="text", x=myeffns.maxSharpe$PortIdx, y=0.5, label="max Sharpe", hjust=-0.1, vjust = 0.5)
    print(p)
  
  #end of if for effFlag="Yes"
  }
  
  if (effFlag=="Yes") {
    my.list <- list(
      c("assets.summary"), as.data.frame(assets.sumplot),
      c("assets.covar"), as.data.frame(assets.covar),
      c("assets.heat"), as.data.frame(assets.heat),
      c("ShortingAllowed.Eff.df"), as.data.frame(myeff),
      c("ShortingAllowed.MaxSharpe.df"), as.data.frame(myeff.maxSharpe),
      c("NoShorting.Eff.df"), as.data.frame(myeffns),
      c("NoShorting.MaxSharpe.df"), as.data.frame(myeffns.maxSharpe)
    )
    
  } else {
    my.list <- list(
      c("assets.summary"), as.data.frame(assets.sumplot),
      c("assets.covar"), as.data.frame(assets.covar),
      c("assets.heat"), as.data.frame(assets.heat)
    )
    
    
  }
    
  return( my.list )  
  
}