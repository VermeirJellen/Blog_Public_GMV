############################################################################
### Perform Global Minimum Variance backtest                           #####
############################################################################
GMVPortfolioBacktest <- function(returns=NULL, assets=NULL, 
                                 lookback=120, 
                                 longOnly=FALSE,
                                 covariance.f="CovClassic", 
                                 max.weight=1,
                                 nrCores=detectCores(), 
                                 plot=TRUE, strategyName="Strategy")
{
  if(is.null(returns))
    returns <- na.omit((assets / lag(assets, k= 1) - 1) * 100)
  
  nrAssets <- ncol(returns)
  
  cl <- makeCluster(nrCores); registerDoParallel(cl)
  clusterEvalQ(cl, eval(parse("config/config.R")))
  clusterExport(cl,c("returns", "longOnly", "nrAssets",
                     "covariance.f", "max.weight"), envir=environment())
  # Calculate weights for out of sample timestamps
  portfolioWeights <- foreach(index.end=lookback:nrow(returns)) %dopar%
  {
    index.start <- index.end-lookback+1
    returns.lookback <- returns[index.start:index.end,]
    
    # Perform GMV optimization: obtain next-day weights
    list(GMVOptimization(returns=returns.lookback, longOnly=longOnly, max.weight=max.weight))
  }
  stopCluster(cl)
  
  #############################################################
  ###### POSTPROCESSING of Results                      #######
  #############################################################
  portfolioWeights <- do.call("rbind", lapply(portfolioWeights, "[[", 1))
  # Convert to xts object
  portfolioWeights <- xts(portfolioWeights, 
                          order.by=index(tail(returns, nrow(portfolioWeights))))
  
  # Allign forecasted weights with realized returns
  portfolioWeights <- lag(portfolioWeights,1)
  # Remove leading NA value
  portfolioWeights <- portfolioWeights[complete.cases(portfolioWeights)]
  # Add column names
  names(portfolioWeights) <- names(returns)
  
  # Calculate the portfolio returns of the strategy
  portfolio.returns <- xts(rowSums(returns[index(portfolioWeights)]*portfolioWeights),
                           order.by=index(portfolioWeights))
  # Add returns and weights to a list
  strategyResults <- list(portfolio.returns, portfolioWeights)
  
  #############################################################
  ###### PLOTTING                                       #######
  #############################################################
  if(plot)
    plotReturns(portfolio.returns, strategyName=strategyName)
  
  return(strategyResults)
}