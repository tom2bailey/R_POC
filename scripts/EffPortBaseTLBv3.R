# EffPortBaseTLB - v3 - base functions for efficient portfolio calculations
# v3 - updated 11/19/2018 - TLB
#
# original quadprog related coded from: http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
#
# Primary functions and data records:
#   effPort_MakeQuadParams: returns list of Amat, bvec, dvec, and meq parameters for quadprod solver from input parameters
#   effPort_MakeQuadResultsMatrix: returns an empty matrix of the correct size, shape, and columns names for frontier results
#   effPort_SinglePoint: returns results for a single point on the efficient frontier
##   effPort_MakeFrontier: returns results for an entire frontier of points
##   effPort_CalcRiskMult: returns risk multiplier to hit near a selected portfolio risk target
# add routines for:
##   routines to sort expected returns and covariances into ascending or descending order of return, or std dev.
##   cleanup results to only those tickers used in a portfolio
##   plot base results
##   standard data record to hold ticker inputs
##   standard data record to hold frontier outputs, including notations
##   standard data record to hold ticker inputs and any number of frontier outputs
##   routines to create the normal set of plots from the above
# Test functions and related stuff
#

# required packages - to be loaded when we source this file
# data.table - required for fread function and data.table structure, and related operations
if (require(data.table) == FALSE) {
  install.packages('data.table')
  library(data.table)
  }

effport_NormalizeNSmooth <-
  function(orig.dt,
           nPtsSmooth,
           nPtsReturnCalc,
           nPtsCovarCalc) {
    # Normalizes data for each ticker to the most recent price value of that ticker.
    # Smooths data to remove high frequency variation (center filter to avoid phase / time shift)
    #   NB: nPtsSmooth for filter must be an odd number for correct smoothing
    # Performs calcuations for smoothing and calculated log-diff returns 
    #   future: allow multiple types of returns to be calculated
    # Returns a list of:
    #   raw (input) data (data.table)
    #   smoothed, augmented data (data.table), 
    #   smoothing value (number of filter points),
    #   type of return calculation (text;  "LogDiff", "ArithReturn", "ArithMultiplier")
    
    #check for nPtsSmooth to be an odd value
    nSmooth <- as.integer(nPtsSmooth)
    if ((nSmooth %% 2) == 0) {
      stop('Error - nPtsSmooth must be odd for correct smoothing - please investigate.')
    }
    
    # create normalized and smoothed values, normalized by most recent date value
    norm.dt <- orig.dt[order(Ticker,-Date)]
    norm.dt[, PriceNorm := Price / Price[1], by = Ticker]
    norm.dt[, PriceNormSmooth := rollmean(PriceNorm, nSmooth, align = "center", fill =
                                            NA), by = Ticker]
    
    # calculate log diff. 
    # NB:  use a 'lead' shift as  the data is currently sorted in descending chronological order.
    norm.dt[, RetLogDiff := log10(PriceNormSmooth) - log10(shift(PriceNormSmooth, 1, type =
                                                                "lead")) , by = Ticker ]
    ilist <- list(
      dt.orig = orig.dt,
      dt.smooth = norm.dt,
      nPtsSmooth = nPtsSmooth,
      Ret.Type = "LogDiff"
    )
    return(ilist)
    
  }


effport_ReadCSV <- 
  function(fileNm) {
    #read CSV file of stacked adjusted daily closing price for any number of tickers
    #expects and checks for 3 columns:
      # Date - date as text, in "%m/%d/%Y" format that can be converted to a true date with as.Date()
      # Ticker - text value of the asset or fund for this row
      # Price - adjusted closing price for the ticker as of this date.
    #Note: we assume adjustments are based on the most recent date in the file
    #Note: all other columns in the CSV file are removed

    #test if the requested file name is valid
    if (file_test("-f", fileNm) == FALSE) { stop('fileNm not found - please investigate.')}

    #read input data
    dt <- fread( fileNm )
    dt$Date <- as.Date(dt$Date, "%m/%d/%Y")
    
    #check for the required column names: Date, Ticker, Price
    if (! all( c("Date", "Ticker", "Price") %in% names(dt) ) ) { 
      stop('Required columms of Date, Ticker, and Price not found - please investigate.')
    }
    
    #drop all other columns
    dt <- dt[,c("Date", "Ticker", "Price")]
    
    return( dt )
    }

effPort_SinglePoint <-
  function(expRet,
           covRet,
           QPparams,
           risk.multiplier) {
    #Output: single row matrix of results (per effPort_MakeQuadResultsMatrix)
    #Inputs:
    
    #prep inputs and create storage for results
    covRet <- as.matrix(covRet)
    n <- ncol(covRet)
    my.QRes <- effPort_MakeQuadResultsMatrix(covRet, 1)
    
    #adjust dvec (from qp params) for risk
    my.dvec <- QPparams$dvec * risk.multiplier
    
    #add calc
    iloop <- 1
    my.sol <-
      solve.QP(
        covRet,
        dvec = my.dvec,
        Amat = QPparams$Amat,
        bvec = QPparams$bvec,
        meq = QPparams$meq
      )
    my.QRes[iloop, "Std.Dev"] <- sqrt(sum(my.sol$solution * colSums((covRet * my.sol$solution))))
    my.QRes[iloop, "Exp.Ret"] <- as.numeric(my.sol$solution %*% expRet)
    my.QRes[iloop, "Sharpe"] <- (my.QRes[iloop, "Exp.Ret"]) / my.QRes[iloop, "Std.Dev"]
    my.QRes[iloop, "Idx"] <- iloop
    my.QRes[iloop, 1:n] <- my.sol$solution
    
    #output results
    return(my.QRes)
  }

effPort_MakeQuadResultsMatrix <- 
  function(covRet, 
           nPoints) {
  # Output: empty matrix of the size and shape to hold efficient frontier points.list of Amat, bvec, dvec, and meq for quadprod function calls
  # Input Args:
  # covRet - m x m matrix of covariances between the assets
  # nPoints - scalar that sets the number of frontier data points to store in the results matrix
  #
  # NB:  covariance are best (perhaps required) to be a matrix, not a dataframe.  Best practice is to call this routine with them already as matrices.
  # for safety, this routine forces them to matrix before other calculations are completed.
  #
  
  covRet <- as.matrix(covRet)
  n <- ncol(covRet)
  
  # Initialize a results matrix to contain allocation and statistics
  iMat <- matrix(nrow = nPoints, ncol = n + 4)
  
  # Now give column names to the output matrix
  colnames(iMat) <- c(colnames(covRet), "Std.Dev", "Exp.Ret", "Sharpe", "Idx")
  return(iMat)
}

effPort_MakeQuadParams <-
  function(expRet,
           covRet,
           short = "no",
           max.allocation = NULL) {
    # Output: list of Amat, bvec, dvec, and meq for quadprod function calls
    # Input Args:
    # expRet - m x 1 vector of expected returns - row order must match the covariance matrix row & column order
    # covRet - m x m matrix of covariances between the assets
    # short - determines if short-selling is allowed: "no" or "yes"; default is "no" (short selling prohibited)
    # max.allocation is the maximum fraction allowed for any one security (reduces concentration) - range 0< x <1
    #
    # NB:  expected returns and covariance are best (perhaps required) to be a matrix, not a dataframe.  Best practice is to call this routine with them already as matrices.
    # for safety, this routine forces them to matrix before other calculations are completed.
    #
    expRet <- as.matrix(expRet)
    covRet <- as.matrix(covRet)
    n <- ncol(covRet)
    
    # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
    Amat <- matrix (1, nrow = n)
    bvec <- 1
    meq <- 1
    
    # Then modify the Amat and bvec if short-selling is prohibited
    if (short == "no") {
      Amat <- cbind(1, diag(n))
      bvec <- c(bvec, rep(0, n))
    }
    
    # And modify Amat and bvec if a max allocation (concentration) is specified
    if (!is.null(max.allocation)) {
      if (max.allocation >= 1 | max.allocation <= 0) {
        stop("max.allocation must be greater than 0 and less than 1")
      }
      if (max.allocation * n < 1) {
        stop("Need to set max.allocation higher; not enough assets to add to 1")
      }
      Amat <- cbind(Amat,-diag(n))
      bvec <- c(bvec, rep(-max.allocation, n))
    }
    
    # Nominal value of dvec: when multiplier equals 1.0.  Should be adjusted when really calling solve.QP.
    dvec <- t(expRet) * 1.0
    
    ilist <- list(
      Amat = Amat,
      bvec = bvec,
      dvec = dvec,
      meq = meq
    )
    return(ilist)
  }


makeEfficientFrontier <- 
  function(expRet, 
            covRet, 
            short="no", 
            max.allocation=NULL, 
            risk.premium.up=.5, 
            risk.increment=.005){
  # Input Args
  # expRet - m x 1 vector of expected returns - row order must match the covariance matrix row & column order
  # covRet - m x m matrix of covariances between the assets
  # short - determines if short-selling is allowed: "no" or "yes"; default is "no" (short selling prohibited)
  # max.allocation is the maximum % allowed for any one security (reduces concentration)
  # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
  # risk.increment is the increment (by) value used in the for loop
  # NB:  expected returns and covariance are best (perhaps required) to be a matrix, not a dataframe.  Best practice is to call this routine with them already as matrices.
  # for safety, this routine forces them to matrix before other calculations are completed.
  
  
  
  # based on code from: http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
  require(quadprog)
  library(quadprog)
  
  #  print("Inputs")
  #  print(expRet)
  #  print(covRet)
  #  str(expRet)
  #  str(covRet)
  
  expRet <- as.matrix(expRet)
  covRet <- as.matrix(covRet)
  n <- ncol(covRet)
  
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
  loops <- risk.premium.up / risk.increment + 1
  loop <- 1
  
  # Initialize a matrix to contain allocation and statistics
  # This is not necessary, but speeds up processing and uses less memory
  eff <- matrix(nrow=loops, ncol=n+4)
  # Now give column names to the output matrix
  colnames(eff) <- c(colnames(covRet), "Std.Dev", "Exp.Ret", "Sharpe", "Idx")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
    # This moves the solution up along the efficient frontier
    dvec <- t(expRet) * i 
    
    #    str(covRet)
    #    str(dvec)
    #    str(Amat)
    #    str(bvec)
    #    str(meq)
    
    sol <- solve.QP(covRet, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covRet * sol$solution))))
    eff[loop,"Exp.Ret"] <- as.numeric(sol$solution %*% expRet)
    eff[loop,"Sharpe"] <- (eff[loop,"Exp.Ret"]) / eff[loop,"Std.Dev"]
    eff[loop,"Idx"] <- loop
    eff[loop,1:n] <- sol$solution
    loop <- loop+1
  }
  
  return(as.data.frame(eff))
}