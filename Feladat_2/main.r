library(dplyr) 
library(magrittr)
library(rio)
library(GGally)
library(network)

X <- list()
X <- loadData(X)
X <- defineParametersdummy(X)

# Ez a szakasz kb 5 percig tart, amíg lefut,
# ennek a változtatásával lehet új window és lag értékek mellett
# elkészíteni az összes eszköz között a korrelációkat.

X <- AllCorrelations(X, windowsize = 150, lagsize = 2)
##################################################################

# A korreláció-idõsorok maximumainak, minimumainak
# és átlagainak ábrázolása az összes eszköz között, hálózatábrán:
correlationNet(X, way = "max")
correlationNet(X, way = "min")
correlationNet(X, way = "mean")

# alapbeállítás: window = 150, lag = 2 mellett legyen fõleg piros

# lagot 2-rõl 1-re csökkentve: megnõnek a korrelációk abszolút értékei
# lagot 2-rõl 3-ra növelve átlagosan szintén inkább nõnek
# lagot 2-rõl 5-re növelve: átlagosan csökkennek,
#   de a minimumok és maximumok abszolút értékben nõnek

# windowt 150-rõl 100-ra csökkentve: átlagosan inkább csökkennek,
#   de a minimumok és maximumok távolodnak egymástól
# windowt 150-rõl 200-ra emelve: maximumok és minimumok absz. értékben
#  csökkennek, de a korrelációk átlaga a hosszú lejáratúak között inkább nõ
