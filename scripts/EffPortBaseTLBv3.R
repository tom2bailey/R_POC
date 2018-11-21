# EffPortBaseTLB - v3 - base functions for efficient portfolio calculations
# v3 - updated 11/19/2018 - TLB
#
# original quadprog related coded from: http://economistatlarge.com/portfolio-theory/r-optimized-portfolio
#
# Functions and data records:
#   effPort_MakeQuadParams: returns list of Amat, bvec, dvec, and meq parameters for quadprod solver from input parameters
#   effPort_MakeQuadResultsFrame: returns an empty data frame of the correct size and columns names for frontier results
#   effPort_SinglePoint: returns results for a single point on the efficient frontier
#   effPort_MakeFrontier: returns results for an entire frontier of points
#   effPort_CalcRiskMult: returns risk multiplier to hit near a selected portfolio risk target
#

effPort_MakeQuadParams <- function(expRet, covRet, short="no", max.allocation=NULL) {
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
    if(max.allocation >= 1 | max.allocation <=0){
      stop("max.allocation must be greater than 0 and less than 1")
    }
    if(max.allocation * n < 1){
      stop("Need to set max.allocation higher; not enough assets to add to 1")
    }
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max.allocation, n))
  }
  
  # Nominal value of dvec: when multiplier equals 1.0.  Should be adjusted when really calling solve.QP.
  dvec <- t(expRet) * 1.0
  
  ilist <- list(Amat=Amat, bvec=bvec, dvec=dvec, meq=meq)
  return( ilist )
}


makeEfficientFrontier <- function (expRet, covRet, short="no", max.allocation=NULL, risk.premium.up=.5, risk.increment=.005){
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