library(dplyr) 
library(magrittr)
library(rio)
library(GGally)
library(network)

X <- list()
X <- loadData(X)
X <- defineParametersdummy(X)

# Ez a szakasz kb 5 percig tart, am�g lefut,
# ennek a v�ltoztat�s�val lehet �j window �s lag �rt�kek mellett
# elk�sz�teni az �sszes eszk�z k�z�tt a korrel�ci�kat.

X <- AllCorrelations(X, windowsize = 150, lagsize = 2)
##################################################################

# A korrel�ci�-id�sorok maximumainak, minimumainak
# �s �tlagainak �br�zol�sa az �sszes eszk�z k�z�tt, h�l�zat�br�n:
correlationNet(X, way = "max")
correlationNet(X, way = "min")
correlationNet(X, way = "mean")

# alapbe�ll�t�s: window = 150, lag = 2 mellett legyen f�leg piros

# lagot 2-r�l 1-re cs�kkentve: megn�nek a korrel�ci�k abszol�t �rt�kei
# lagot 2-r�l 3-ra n�velve �tlagosan szint�n ink�bb n�nek
# lagot 2-r�l 5-re n�velve: �tlagosan cs�kkennek,
#   de a minimumok �s maximumok abszol�t �rt�kben n�nek

# windowt 150-r�l 100-ra cs�kkentve: �tlagosan ink�bb cs�kkennek,
#   de a minimumok �s maximumok t�volodnak egym�st�l
# windowt 150-r�l 200-ra emelve: maximumok �s minimumok absz. �rt�kben
#  cs�kkennek, de a korrel�ci�k �tlaga a hossz� lej�rat�ak k�z�tt ink�bb n�
