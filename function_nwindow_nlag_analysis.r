# Ennek a f�ggv�nynek a m�k�d�se a function_dynamic_correlation 
# f�ggv�nyen alapul, annak bet�lt�se el�zetes felt�tel.
# A f�ggv�ny egyszerre t�bb ablakm�ret �s k�sleltet�s mellett
# kisz�molja a korrel�ci� vektorokat.

# Bemeneti v�ltoz�k:
# x1, x2: A k�t nyers �rfolyamsor
# start_date, end_date: a vizsg�lt peri�dus eleje �s v�ge,
# "�V-H�-NAP" (string) form�tumban
# window, lag: az ablak �s a k�sleltet�s �rt�ke, mindkett�
# lehet tetsz�leges hossz�s�g� vektor is

# A f�ggv�ny outputja egy data frame, amely oszloponk�nt tartalmazza
# a korrel�ci�s vektorokat. P�ld�ul ha 2 window �s 2 lag �rt�ket
# adunk meg, ez egy 4 oszlopos DF, amely 
# els� oszlopa a window[1], lag[1]; 
# m�sodik oszlopa a window[1], lag[2];
# harmadik oszlopa a window[2], lag[1];
# negyedik oszlopa a window[2], lag[2] melletti 
# korrel�ci�-vektorokat tartalmazza.

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
      # a egy egyszint� lista, annyi eleme van, ah�ny k�l�nb�z�
      # lag �rt�k a bemenet
    }
    
    b[[i]]<- a
    # b egy k�tszint� lista: az els� szinten vannak az
    # egyes ablakm�retek, azon bel�l pedig a k�l�nb�z� k�sleltet�sek
  }
  # A b lista konvert�l�sa data frame-�
  A <- sapply(b, FUN = function(x) {x}, simplify = T)  
  M <- plyr::ldply(A, rbind) %>% t()
  
  return(M)
}
