# Ez a függvény ábrázolja a korrelációs idősorok
# maximumát, minimumát és átlagát hálózati ábrán.

# A háló színe azt jelenti, hogy ott a korreláció
# maximumának/minimumának/átlagának abszolút értéke éppen az általunk kijelölt
# (alsó és felső) határokon kívül, vagy azok közé esik.
# A határokat úgy állítottuk be, hogy a
# window = 150, lag = 2 értéknél essen a legtöbb abszolút érték a határok közé.

correlationNet = function(X, way = "max"){
  
    maxes <- apply(X$AllCorr,2,max)
    mins <- apply(X$AllCorr,2,min)
    means <- apply(X$AllCorr,2,mean)
    
  if(way == "max"){  
    netbase <- network(X$edges, directed = T) %>% 
      set.edge.attribute(.,"colors", value =  
                    ifelse( abs(maxes) < 0.18, 
                    ifelse( abs(maxes) < 0.14,"green" ,"red" ),"blue" )) %>%
      delete.vertices(., vid = 1)
  
  baseplot <-  ggnet2(netbase, label = colnames(X$LAST[2:25]),
           mode = "circle", edge.color = "colors", 
           color = "
Denotes the asset #
while c denotes the
          maxes
of the correlations." ,
           node.label = T, node.alpha = 0.4,
           arrow.size = 4, arrow.gap = 0.05,
           color.legend = "Red: 0.18 > |c| >= 0.14,

    Green: |c| < 0.14,

      Blue: |c| >= 0.18" )      
  } else if(way == "min") {
    
    netbase <- network(X$edges, directed = T) %>% 
      set.edge.attribute(.,"colors", value =  
                           ifelse( abs(mins) < 0.2, 
                                   ifelse( abs(mins) < 0.15,"green" ,"red" ),"blue" )) %>%
      delete.vertices(., vid = 1)
    
    baseplot <-  ggnet2(netbase, label = colnames(X$LAST[2:25]) ,
                        mode = "circle", edge.color = "colors", 
                        color = "
Denotes the asset #
while c denotes the
          mins
of the correlations." ,
                        node.label = T, node.alpha = 0.35,
                        arrow.size = 4, arrow.gap = 0.05,
                        color.legend = "Red: 0.2 > |c| >= 0.15,
                        
        Green: |c| < 0.15,
                        
         Blue: |c| >= 0.2" )
    
  } else if(way == "mean") {
    netbase <- network(X$edges, directed = T) %>% 
      set.edge.attribute(.,"colors", value =  
                           ifelse( abs(means) < 0.015, 
                                   ifelse( abs(means) < 0.01,"green" ,"red" ),"blue" )) %>%
      delete.vertices(., vid = 1)
    
    baseplot <-  ggnet2(netbase, label = colnames(X$LAST[2:25]) ,
                        mode = "circle", edge.color = "colors", 
                        color = "
Denotes the asset #
while c denotes the
          means
of the correlations." ,
                        node.label = T, node.alpha = 0.4,
                        arrow.size = 4, arrow.gap = 0.05,
                        color.legend = "Red: 0.015 > |c| >= 0.01,
                        
      Green: |c| < 0.01,
                        
        Blue: |c| >= 0.015" )
  } else{
    return( paste("Make sure the second argument is one of the following: ",
            "'max'","'min'", "'mean'"))
  }
  return(plot(baseplot))
}


