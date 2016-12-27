rm(list=ls())

# setwd(current.working.directory)
###########################################################################
############## Load demo packages, functions, and data          ###########
###########################################################################
# Load packages and import functions: MUST be initially executed.
source("config/config.R")

SPDR <- readRDS(file="data/SPDR.rds")
SPDR.returns <- na.omit((SPDR / lag(SPDR, k= 1) - 1) * 100)

BEL20 <- readRDS(file="data/BEL20.rds")
BEL20.returns <- na.omit((BEL20 / lag(BEL20, k= 1) - 1) * 100)

maxCores <- detectCores()
nrCores = ifelse(maxCores>1, maxCores-1, 1)

###########################################################################
############# Plot the underlying assets ##################################
###########################################################################
nrAssets <- ncol(SPDR)
par(mfrow=c(rep(ceiling(sqrt(nrAssets)),2)))
res <- sapply(1:nrAssets, function(x) plot(time(SPDR[,x]),
                                           coredata(SPDR[,x]), main=names(SPDR)[x],type="l",
                                           xlab="Time",ylab="Price"))


nrAssets <- ncol(BEL20)
par(mfrow=c(rep(ceiling(sqrt(nrAssets)),2)))
res <- sapply(1:nrAssets, function(x) plot(time(BEL20[,x]),
                                           coredata(BEL20[,x]), main=names(BEL20)[x],type="l",
                                           xlab="Time",ylab="Price"))
#################################################################################
############################ GMV Settings #######################################
#################################################################################
gmv.lookback=120
gmv.maxWeight=0.3
gmv.longOnly=FALSE
gmv.estimator="CovClassic"
#################################################################################
############################ GMV BACKTEST SPDR ##################################
#################################################################################
# Equal weight allocation benchmark for SPDR portfolio
equalWeights <- xts(matrix(1/ncol(SPDR.returns), nrow=nrow(SPDR.returns),
                      ncol=ncol(SPDR.returns)), order.by=index(SPDR.returns))
SPDR.EqualWeights <- xts(rowSums(SPDR.returns[index(equalWeights)]*equalWeights), 
                                         order.by=index(equalWeights))

# GMV strategy for SPDR portfolio
SPDR.GMV <- GMVPortfolioBacktest(returns=SPDR.returns, 
                                 max.weight=gmv.maxWeight,
                                 covariance.f=gmv.estimator, 
                                 lookback=gmv.lookback,
                                 longOnly=gmv.longOnly, 
                                 nrCores=nrCores,
                                 plot=TRUE, strategyName="SPDR GMV")

# Compare equal weight strategy with GMV strategy for SPDR portfolio
plotReturns(returns=SPDR.EqualWeights, 
            strategyName="SPDR Equal Weights",
            returns.compare=SPDR.GMV[[1]], 
            strategyName.compare="SPDR GMV")

#################################################################################
############################ GMV BACKTEST BEL20 #################################
#################################################################################

# Equal weight allocation benchmark for BEL20 portfolio
equalWeights <- xts(matrix(1/ncol(BEL20.returns), nrow=nrow(BEL20.returns),
                      ncol=ncol(BEL20.returns)), order.by=index(BEL20.returns))
BEL20.EqualWeights <- xts(rowSums(BEL20.returns[index(equalWeights)]*equalWeights), 
                         order.by=index(equalWeights))

# GMV strategy for BEL20 portfolio
BEL20.GMV <- GMVPortfolioBacktest(returns=BEL20.returns, 
                                  max.weight=gmv.maxWeight,
                                  covariance.f=gmv.estimator, 
                                  lookback=gmv.lookback,
                                  longOnly=gmv.longOnly, 
                                  nrCores=nrCores,
                                  plot=TRUE, strategyName="BEL20 GMV")

# Compare equal weight strategy with GMV for BEL20 portfolio
plotReturns(returns=BEL20.EqualWeights, 
            strategyName="BEL20 Equal Weights",
            returns.compare=BEL20.GMV[[1]], 
            strategyName.compare="BEL20 GMV")