defineParametersdummy <- function(X){
  X$CorrelationWindow <- c(100)
  X$CorrelationLag <- c(1)
  X$Maturity <- c(2,8)
  return(X)
}
