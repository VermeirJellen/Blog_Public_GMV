## gmv optimization
## Function expects return timeseries xts-object as input
GMVOptimization <- function(returns, covariance.f="CovClassic", longOnly=FALSE, max.weight=1)
{
  estimation <- do.call(covariance.f, 
                        list(x=coredata(returns)))
  covariance.matrix <- rrcov::getCov(estimation)
  
  # The quadratic solver below implements the dual method of Goldfarb and Idnani (1982, 1983) 
  # for solving quadratic programming problems of the form min(-d^T b + 1/2 b^T D b) 
  # with the constraints A^T b >= b_0.
  #
  # Here, we mimize b^T D b, with D the covariance matrix and b the target weights
  # Constraints are full investment of capital and/or long only
  nrMarginals <- ncol(covariance.matrix)
  # no linear components in target function
  dvec <- rep.int(0, nrMarginals)
  
  # Fully Invested (equality constraint)
  a <- rep.int(1, nrMarginals)
  b <- 1
  
  if(longOnly)
  {
    # Long only (>= constraint)
    a2 <- diag(nrMarginals)
    b2 <- rep.int(0, nrMarginals)
    
    a <- rbind(a, a2)
    b <- c(b, b2)
  }
  
  # Weights greater than -max.weight
  a3 <- diag(nrMarginals)
  b3 <- rep.int(-max.weight, nrMarginals)
  
  # Weights smaller than max.weight
  a4 <- -diag(nrMarginals)
  b4 <- rep.int(-max.weight, nrMarginals)
  
  Amat <- t(rbind(a, a3, a4)) # This matrix will be transposed again
  bvec <- c(b, b3, b4)
  
  # Solve the quadratic problem
  gmv <- solve.QP(Dmat=covariance.matrix, dvec=dvec, Amat=Amat,
                  bvec=bvec, meq=1) # meq = 1 equality constraint
  return(gmv$solution)
}