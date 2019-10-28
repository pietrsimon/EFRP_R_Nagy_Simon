# Ennek a függvénynek a mûködése a function_nwindow_nlag_analysis
# függvényen alapul, annak betöltése elõzetes feltétel.
# A függvény egyszerre több ablakméret és késleltetés mellett
# ábrázolja a dinamikus korrelációt.

# Bemeneti változók:
# x1, x2: A két nyers árfolyamsor
# start_date, end_date: a vizsgált periódus eleje és vége,
# "ÉV-HÓ-NAP" (string) formátumban
# window, lag: az ablak és a késleltetés értéke, mindkettõ
# lehet tetszõleges hosszúságú vektor is

# A függvény outputja egy ábra, amely a különbözõ parametrizálások
# melletti dinamikus korrelációt mutatja be x1 és x2 között.

# A különbözõ lag és window értékek melletti dinamikus korrelációk
# nem ugyanolyan hosszú vektorokat adnak eredményül.
# A függvény több ilyet gyûjt egy mátrixba, és az üres helyeket
# NA-val tölti fel, amelyek nem ábrázolhatók,
# így az ábrázolás során sok warning message-t generálódik.

correlation_plot <- function(x1, x2, start_date, end_date,
                            window = 100, lag = 1){
  
  # összeköti a dátumok vektorát a korrelációk vektoraiból
  # alkotott DF-el, és NA-val tölti ki az eltérõ vektorhosszokból
  # adódó üres helyeket
  plot_DF <- as.Date(dynamic_correlation(x1,x2, start_date,
                    end_date, window, lag)[[2]]) %>% 
    rowr::cbind.fill(analysis(x1, x2,start_date,
                    end_date, window, lag),
                     fill = NA)
  
  # az a mátrix csak azért kell, hogy a korreláció-vektorok
  # (iteratívan) el legyenek nevezve
  a <- matrix(1:(length(window)*length(lag)), nrow = length(window))
  
  for(i in 1:length(window)){
    for (j in 1:length(lag)){
      a[i,j] <- paste0("Window",window[i],"Lag", lag[j])
    } 
  }
  
  b <- c(t(a))
  colnames(plot_DF) <- c("date", b) 
  # ez maga az oszlopok elnevezése a hozzájuk tartozó
  # window és lag értékek szerint
  
  # az egyszerû ábrázolás kedvéért a melt paranccsal rendezi
  # az adatokat
  plot_melted <- reshape2:: melt(plot_DF, id.var = "date" )
  
  gg <- ggplot( plot_melted, aes(x = date, y = value, col = variable) ) +
    geom_line() +
    labs( title = "Dinamikus korreláció különbözõ ablak és késleltetés értékek mellett",
          x = "Dátum",
          y = "Korreláció") +
    theme( panel.background = element_blank())
  
  return(plot(gg))
}
