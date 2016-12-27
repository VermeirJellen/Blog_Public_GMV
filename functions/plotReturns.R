############################################################################
### A few basic performance measurement functions.                      ####
############################################################################
cumulativeReturns <- function(x){ return(cumprod(1+x/100)) }
customSharpe <- function(x){round(sqrt(252)*mean(x)/sd(x),digits=4)}
customAPR <- function(x){round((prod(1+x/100)^(252/length(x))-1)*100,digits=4)}

############################################################################
# Plot and/or compare trading performance of 1 or 2 return series objects ##
############################################################################
plotReturns <- function(returns, strategyName="Strategy",
                        returns.compare=NULL, strategyName.compare=NULL)
{
  par(mfrow=c(1,1))
  if(!is.null(returns.compare))
    par(mfrow=c(2,1))
  
  cumprod <- cumulativeReturns(returns)
  sharpe <- customSharpe(returns)
  maxDD <- maxDrawdown(returns/100)
  APR <- customAPR(returns)
  
  ylim <- c(min(cumprod), max(cumprod))
  plot(time(cumprod), coredata(cumprod), main=paste(strategyName),
       xlab="Time",ylab="Cumulative Returns",type='l',col="black",ylim=ylim)
  legend("topleft",legend= c(paste("Sharpe:",round(sharpe, digits=2)),
                             paste("MaxDD: ", round(maxDD*100, digits=2),"%",sep=""),
                             paste("APR: ", round(APR, digits=2),"%",sep="")))
  
  if(!is.null(returns.compare))
  {
    cumprod <- cumulativeReturns(returns.compare)
    sharpe <- customSharpe(returns.compare)
    maxDD <- maxDrawdown(returns.compare/100)
    APR <- customAPR(returns.compare)
    
    ylim <- c(min(cumprod), max(cumprod))
    plot(time(cumprod), coredata(cumprod), main=paste(strategyName.compare),
         xlab="Time",ylab="Cumulative Returns",type='l',col="black",ylim=ylim)
    legend("topleft",legend= c(paste("Sharpe:",round(sharpe, digits=2)),
                               paste("MaxDD: ", round(maxDD*100, digits=2),"%",sep=""),
                               paste("APR: ", round(APR, digits=2),"%",sep="")))
  }
}