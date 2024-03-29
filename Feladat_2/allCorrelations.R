# Ez a függvény kiszámolja a tetszőleges window és lag értékek mellett
# az összes eszköz közötti korrelációs idősorokat.

# A keletkező mátrix kb 10MB-os, ezért ez nagyon lassan fut le.

AllCorrelations = function(X, windowsize = 150, lagsize = 2){
  
  X$edges <- cbind( rep(2:length(X$LAST), each = (length(X$LAST)-1)), 
                    rep(2:length(X$LAST), length(X$LAST)-1))
  X$edges <- X$edges[ X$edges[,1]!=X$edges[,2], ]  
  
  X$CorrelationWindow <- windowsize
  X$CorrelationLag <- lagsize
  
  X$AllCorr <- apply(X$edges,1, FUN = function(y){ 
    X$Maturity  <- y
    z = correlationAnalysis(X)
    return(z)
  })
  
  return(X)
}
