# Dinamikus korreláció elemzése a WTI adatbázison.
# Az elemzéshez a következõ fájlokban lévõ függvények szükségesek:
# function_load_dataset.R
# function_dynamic_correlation.R
# function_nwindow_nlag_analysis.R
# function_correlation_plot.R

# Az elemzés során a tidyverse-en és a beépített package-eken
# kívül a "rowr" package-t használtuk.
install.packages("rowr")
 
# Az alábbiakat globálisan is betöltöttük: 
library(tidyr)
library(ggplot2)

WTI <- NULL 
# Ez a listaobjektum fogja tartalmazni a nyers adatokat, és az
# elemzés eredményeit is.


# Az adatok betöltése

WTI$rawdata <- load_dataset()  # alapértelmezés: "WTI2.xlsx" 
# más elérési útvonal esetén az elérési útvonal megadásával futtatható


# A tetszõleges paraméterek melletti dinamikus korrelációk kiszámolása

WTI$correlations <- analysis(WTI$rawdata$CL1, WTI$rawdata$CL2,
         "2014-01-01", "2016-08-20", window = c(100, 150), lag = c(1,2))


# A tetszõleges paraméterek melletti dinamikus korrelációk ábrázolása

correlation_plot(WTI$rawdata$CL1, WTI$rawdata$CL2,
         "2014-01-01", "2016-08-20", window = c(100, 150), lag = c(1,2))






