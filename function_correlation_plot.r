# Ennek a f�ggv�nynek a m�k�d�se a function_nwindow_nlag_analysis
# f�ggv�nyen alapul, annak bet�lt�se el�zetes felt�tel.
# A f�ggv�ny egyszerre t�bb ablakm�ret �s k�sleltet�s mellett
# �br�zolja a dinamikus korrel�ci�t.

# Bemeneti v�ltoz�k:
# x1, x2: A k�t nyers �rfolyamsor
# start_date, end_date: a vizsg�lt peri�dus eleje �s v�ge,
# "�V-H�-NAP" (string) form�tumban
# window, lag: az ablak �s a k�sleltet�s �rt�ke, mindkett�
# lehet tetsz�leges hossz�s�g� vektor is

# A f�ggv�ny outputja egy �bra, amely a k�l�nb�z� parametriz�l�sok
# melletti dinamikus korrel�ci�t mutatja be x1 �s x2 k�z�tt.

# A k�l�nb�z� lag �s window �rt�kek melletti dinamikus korrel�ci�k
# nem ugyanolyan hossz� vektorokat adnak eredm�ny�l.
# A f�ggv�ny t�bb ilyet gy�jt egy m�trixba, �s az �res helyeket
# NA-val t�lti fel, amelyek nem �br�zolhat�k,
# �gy az �br�zol�s sor�n sok warning message-t gener�l�dik.

correlation_plot <- function(x1, x2, start_date, end_date,
                            window = 100, lag = 1){
  
  # �sszek�ti a d�tumok vektor�t a korrel�ci�k vektoraib�l
  # alkotott DF-el, �s NA-val t�lti ki az elt�r� vektorhosszokb�l
  # ad�d� �res helyeket
  plot_DF <- as.Date(dynamic_correlation(x1,x2, start_date,
                    end_date, window, lag)[[2]]) %>% 
    rowr::cbind.fill(analysis(x1, x2,start_date,
                    end_date, window, lag),
                     fill = NA)
  
  # az a m�trix csak az�rt kell, hogy a korrel�ci�-vektorok
  # (iterat�van) el legyenek nevezve
  a <- matrix(1:(length(window)*length(lag)), nrow = length(window))
  
  for(i in 1:length(window)){
    for (j in 1:length(lag)){
      a[i,j] <- paste0("Window",window[i],"Lag", lag[j])
    } 
  }
  
  b <- c(t(a))
  colnames(plot_DF) <- c("date", b) 
  # ez maga az oszlopok elnevez�se a hozz�juk tartoz�
  # window �s lag �rt�kek szerint
  
  # az egyszer� �br�zol�s kedv��rt a melt paranccsal rendezi
  # az adatokat
  plot_melted <- reshape2:: melt(plot_DF, id.var = "date" )
  
  gg <- ggplot( plot_melted, aes(x = date, y = value, col = variable) ) +
    geom_line() +
    labs( title = "Dinamikus korrel�ci� k�l�nb�z� ablak �s k�sleltet�s �rt�kek mellett",
          x = "D�tum",
          y = "Korrel�ci�") +
    theme( panel.background = element_blank())
  
  return(plot(gg))
}
