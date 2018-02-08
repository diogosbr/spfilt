#devtools::setup()

library(devtools)

#criou a estrutura do pacote
create("spfilt", rstudio = FALSE)

devtools::document()
#devtools::load_all()

devtools::install_github("diogosbr/spfilt", ref = "teste1")

require(spfilt)

?filt
?rm_accent
?br_mun

#Obtendo coordenadas da esp?cie/grupo de interesse
mani=dismo::gbif("euterpe edulis")
manimax=mani[,c("species","lon","lat", "municipality", "adm1")]
manimax=na.exclude(manimax)

pts = manimax

head(pts)


#testando as funções

?rm_accent
?filt

rm_accent(pts$municipality)

results = filt(manimax)
head(results,10)
table(results$status)
class(manimax)



library(rgdal)
library(dismo)

#br_mun = readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp", encoding = "UTF-8")
#br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

use_data(br_mun, overwrite = T)


check()
?check

aa = as.vector(br_mun$NOMEMUNIC)
Encoding(aa)="UTF-8"
Encoding(aa)


