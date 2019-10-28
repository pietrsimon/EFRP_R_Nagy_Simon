# Ez a függvény két nyers árfolyamsorból loghozamokat csinál, és 
# kiszámolja ezek között a dinamikus korrelációt, adott kezdõ dátum,
# végdátum, ablakméret és késleltetés értékek mellett.

# Bemeneti változók:
# x1, x2: A két nyers árfolyamsor
# start_date, end_date: a vizsgált periódus eleje és vége,
# "ÉV-HÓ-NAP" (string) formátumban
# window, lag: az ablak és a késleltetés értéke, mindkettõ
# egy szám, nem pedig vektor

# A függvény outputja egy 2 elemû lista, elsõ eleme a
# korrelációk vektora, második a dátumok vektora.

dynamic_correlation <- function(x1, x2, start_date, end_date,
                               window = 100, lag = 1){
  
  X <- cbind(WTI$rawdata$Date[2:length(WTI$rawdata$Date)],
            diff(log(x1)),diff(log(x2)))
  
  start <- as.numeric(as.POSIXct(start_date, format="%Y-%m-%d"))
  
  end <- as.numeric(as.POSIXct(end_date, format="%Y-%m-%d"))
  
  correlations <- list(NULL)
  
  X <- X[(start <= X[,1]) & X[,1] <= end , ]
  
  for(k in 1:(length(X[,1])-window-lag+1)){
    
      a <- X[(k+lag): (k+lag+window-1),2]
      
      b <- X[k:(k+window-1),3]
      
      correlations[[1]][k] <- cor(a,b)
  }
  correlations[[2]] <- as.POSIXct(X[,1], origin = "1970-01-01")
  
  return(correlations)
}



