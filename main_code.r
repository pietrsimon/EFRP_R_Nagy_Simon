# Dinamikus korrel�ci� elemz�se a WTI adatb�zison.
# Az elemz�shez a k�vetkez� f�jlokban l�v� f�ggv�nyek sz�ks�gesek:
# function_load_dataset.R
# function_dynamic_correlation.R
# function_nwindow_nlag_analysis.R
# function_correlation_plot.R

# Az elemz�s sor�n a tidyverse-en �s a be�p�tett package-eken
# k�v�l a "rowr" package-t haszn�ltuk.
install.packages("rowr")
 
# Az al�bbiakat glob�lisan is bet�lt�tt�k: 
library(tidyr)
library(ggplot2)

WTI <- NULL 
# Ez a listaobjektum fogja tartalmazni a nyers adatokat, �s az
# elemz�s eredm�nyeit is.


# Az adatok bet�lt�se

WTI$rawdata <- load_dataset()  # alap�rtelmez�s: "WTI2.xlsx" 
# m�s el�r�si �tvonal eset�n az el�r�si �tvonal megad�s�val futtathat�


# A tetsz�leges param�terek melletti dinamikus korrel�ci�k kisz�mol�sa

WTI$correlations <- analysis(WTI$rawdata$CL1, WTI$rawdata$CL2,
         "2014-01-01", "2016-08-20", window = c(100, 150), lag = c(1,2))


# A tetsz�leges param�terek melletti dinamikus korrel�ci�k �br�zol�sa

correlation_plot(WTI$rawdata$CL1, WTI$rawdata$CL2,
         "2014-01-01", "2016-08-20", window = c(100, 150), lag = c(1,2))






