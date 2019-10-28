# Ennek a függvénynek a mûködése a function_dynamic_correlation 
# függvényen alapul, annak betöltése elõzetes feltétel.
# A függvény egyszerre több ablakméret és késleltetés mellett
# kiszámolja a korreláció vektorokat.

# Bemeneti változók:
# x1, x2: A két nyers árfolyamsor
# start_date, end_date: a vizsgált periódus eleje és vége,
# "ÉV-HÓ-NAP" (string) formátumban
# window, lag: az ablak és a késleltetés értéke, mindkettõ
# lehet tetszõleges hosszúságú vektor is

# A függvény outputja egy data frame, amely oszloponként tartalmazza
# a korrelációs vektorokat. Például ha 2 window és 2 lag értéket
# adunk meg, ez egy 4 oszlopos DF, amely 
# elsõ oszlopa a window[1], lag[1]; 
# második oszlopa a window[1], lag[2];
# harmadik oszlopa a window[2], lag[1];
# negyedik oszlopa a window[2], lag[2] melletti 
# korreláció-vektorokat tartalmazza.

analysis <- function(x1, x2, start_date, end_date,
                    window = 100, lag = 1){
  a <- list(0)
  b <- list(0)
  
  for(i in 1:length(window) ){
    
    w <- window[i]
    
    for(j in 1:length(lag)){
      l <- lag[j]
      
      a[[j]] <- dynamic_correlation(x1, x2, start_date, end_date,
                                   window = w, lag = l)[[1]]
      # a egy egyszintû lista, annyi eleme van, ahány különbözõ
      # lag érték a bemenet
    }
    
    b[[i]]<- a
    # b egy kétszintû lista: az elsõ szinten vannak az
    # egyes ablakméretek, azon belül pedig a különbözõ késleltetések
  }
  # A b lista konvertálása data frame-é
  A <- sapply(b, FUN = function(x) {x}, simplify = T)  
  M <- plyr::ldply(A, rbind) %>% t()
  
  return(M)
}
