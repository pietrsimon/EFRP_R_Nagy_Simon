# Ez a f�ggv�ny k�t nyers �rfolyamsorb�l loghozamokat csin�l, �s 
# kisz�molja ezek k�z�tt a dinamikus korrel�ci�t, adott kezd� d�tum,
# v�gd�tum, ablakm�ret �s k�sleltet�s �rt�kek mellett.

# Bemeneti v�ltoz�k:
# x1, x2: A k�t nyers �rfolyamsor
# start_date, end_date: a vizsg�lt peri�dus eleje �s v�ge,
# "�V-H�-NAP" (string) form�tumban
# window, lag: az ablak �s a k�sleltet�s �rt�ke, mindkett�
# egy sz�m, nem pedig vektor

# A f�ggv�ny outputja egy 2 elem� lista, els� eleme a
# korrel�ci�k vektora, m�sodik a d�tumok vektora.

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



