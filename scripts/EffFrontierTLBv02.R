EffFrontier.AnalyzeScenarios <- function ( 
    stackedTicker, #data.frame/data.table of m rows, with columns of Date, Ticker, Price
        # Date is only used to sort in chronological order and x-axis of charts, so can be any variable type
        # Ticker - symbol of security or asset
        # Price - price of the asset
        # Any additional columns are ignored.
    periodsPerYear, # Price points per year - 12 for monthly data, 252 for daily data
    listCalc, # list(1..n) of calc method values to use - names are ignored - values: "Return", "Multiplier", "LogDiff"
    listScale, # list(1..n) of scaling methods requested - values: "Native", "AnnualizedReturn"
    listRiskFree, # list(1..n) of risk free asset returns, annualized basis, decimal number.  E.g. .01 mean 1% annual return
    listNumSamples, # list(1..n of lists - two named members required)
        #nExpRet - number of data points to include when calculating expected returns
        #nExpCovar - number of data points to include when calculating covariances and std deviations
    listShortFlag, # list(1..n) of "Yes" or "No" if shorting is allowed or not
    listTopN, # list(1..n) number of highest return assets to include for frontier calculations; values must be >1
    listPriceSmoothing # list(1..n) of points for price smoothing;  must be odd numbers
    ){

  #xxxxx 
  # reconsider which factors are full factorial combinations, and which are grouped to be specified
    #possible individual factors (full factorial)
      # TopN
    #All else to be specified
    #????
  
  require(data.table)
  library(data.table)


  #check / confirm valid inputs values
  for (i in listCalc) {
      if (!( (as.character(i)=="Return") || (as.character(i)=="Multiplier") || (i=="LogDiff") ) ) {
      #raise some kind of message/error
      stop(paste("At least one Calculation Method value is not correct.  Please investigate: value='", i, "'", sep = ""))
    }
  }
  
  for (i in listScale) {
    if (!( (i=="Native") || (i=="AnnualizedReturn") ) ) {
      #raise some kind of message/error
      stop(paste("At least one Output Scaling value is not correct.  Please investigate: value='", i, "'", sep = ""))
    }
  }

  for (i in listShortFlag) {
    if (!( (i=="No") || (i=="Yes") ) ) {
      #raise some kind of message/error
      stop(paste("At least one Shorting Flag value is not correct.  Please investigate: value='", i, "'", sep = ""))
    }
  }
  
  for (i in listPriceSmoothing) {
    if ( (as.integer(i) %% 2)==0 ) {
      #values must be odd.  If values are even, raise some kind of message/error
      stop(paste("At least one Price Smoothing value is not correct. Values must be odd.  Please investigate: value='", i, "'", sep = ""))
    }
  }
  
  
  
  #No input value check at this time
  #listRiskFree
  #listNumSamples
  #listTopN

  # get number of permutations
  nCalc <- length(listCalc)
  nScale <- length(listScale)
  nRiskFree <- length(listRiskFree)
  nNumSamples <- length(listNumSamples)
  nShortFlag <- length(listShortFlag)
  nTopN <- length(listTopN)
  nPriceSmoothing <- length(listPriceSmoothing)
  nPermut <- nCalc*nScale*nRiskFree*nNumSamples*nShortFlag*nTopN*nPriceSmoothing
  
  print(paste("nPermutations: =", nPermut, sep=""))
  
  
  #create loop structures
  ##remember/consider to move Sharpe ratio calculation outside of eff routine, ditto for max Sharpe calculation  ? but this might limit auto adjust for risk steps in frontier calcs???
  
  # convert to data.table object
  stacked.dt <- as.data.table(stackedTicker)
  nSmooth <- 3
  
  # create indexed values, normalized by most recent date value
  stacked.dt <- stacked.dt[order(Ticker, -Date)]
  stacked.dt[, IdxPrice := Price/Price[1], by = Ticker]
  stacked.dt[, SmthIdxPrice := rollmean(IdxPrice, nSmooth, align="center", fill=NA), by = Ticker]
  # calculate arithmetic returns, multiplier, and log diff. NB:  use a 'lead' shift as  the data is currently sorted in descending chronological order.
  stacked.dt[, Returns := SmthIdxPrice / shift(SmthIdxPrice, 1, type="lead") - 1, by = Ticker]
  stacked.dt[, Multiplier := SmthIdxPrice / shift(SmthIdxPrice, 1, type="lead") , by = Ticker]
  stacked.dt[, LogTen := log10(SmthIdxPrice) , by = Ticker]
  stacked.dt[, LogTenDiff := log10(SmthIdxPrice) - log10(shift(SmthIdxPrice, 1, type="lead")) , by = Ticker]
  
  #drop any rows with NA that might have resulted from smoothing or other calculations
  stacked.dt <- na.omit(stacked.dt)
  
  stop("End of new code")

  
  #xxxxxxxxx
  #determine which type of "return" to calculate: "Return", "Multiplier", or "LogDiff"
  if (inputFactors$calcApproach)
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






EffFrontier.Calculate <- function (expRet, expCov, RiskFreeReturn, short="no", max.allocation=NULL, risk.premium.max=.5, risk.increment=.005){
# Input Args
  # expRet - m x 1 vector of expected returns - row order must match the covariance matrix row & column order
  # expCov - m x m matrix of covariances between the assets
  # RiskFreeReturn - expected return of a risk free asset for Sharpe Ratio calculation
  #   NOTE: the base "units" of expRet, expCov, and RiskFreeReturn must be the same (e.g., daily multiplier, daily log difference, etc.)
  # short - determines if short-selling is allowed: "no" or "yes"; default is "no" (short selling prohibited)
  # max.allocation is the maximum % allowed for any one security (reduces concentration)
  # risk.premium.max is the upper limit of the risk premium modeled (see for loop below)
  # risk.increment is the increment (by) value used in the for loop

#Output:
  # List of:
  # eff = data frame of efficient frontier points: portfolio weights, port return, port std dev, Sharpe Ratio
  # maxSharpe = data of max Sharpe ratio point on the frontier
  
# based on code from: http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
  require(quadprog)
  library(quadprog)
  
#  print("Inputs")
#  print(expRet)
#  print(expCov)
#  str(expRet)
#  str(expCov)
  n <- ncol(expCov)
  
  # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
  Amat <- matrix (1, nrow=n)
  bvec <- 1
  meq <- 1
  
  # Then modify the Amat and bvec if short-selling is prohibited
  if(short=="no"){
    Amat <- cbind(1, diag(n))
    bvec <- c(bvec, rep(0, n))
  }
  
  # And modify Amat and bvec if a max allocation (concentration) is specified
  if(!is.null(max.allocation)){
    if(max.allocation > 1 | max.allocation <0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Calculate the number of loops based on how high to vary the risk premium and by what increment
  loops <- risk.premium.max / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+4)
  # Now I need to give the matrix column names
  colnames(eff) <- c(colnames(expCov), "Std.Dev", "Exp.Return", "Sharpe", "Idx")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.max, by=risk.increment)){
    # This moves the solution up along the efficient frontier
    dvec <- t(expRet) * i 
    sol <- solve.QP(expCov, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((expCov * sol$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% expRet)
    eff[loop,"Sharpe"] <- (eff[loop,"Exp.Return"]-RiskFreeReturn) / eff[loop,"Std.Dev"]
    eff[loop,"Idx"] <- loop
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
eff.maxSharpe <- eff[eff$Sharpe==max(eff$Sharpe),]
eff.maxSharpe <- eff.maxSharpe[1,] # trim to 1 row max, just in case two rows matched max Sharpe value

tmpList <- list(eff=as.data.frame(eff), maxSharpe=as.data.frame(eff.maxSharpe), riskFree=RiskFreeReturn)

return(tmpList)
}