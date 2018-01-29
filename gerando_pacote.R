#devtools::setup()

library(devtools)

create("spfilt", rstudio = FALSE)

devtools::document()
#devtools::load_all()
install_github("diogosbr/spfilt")

require(spfilt)



#Obtendo coordenadas da esp?cie/grupo de interesse
mani=dismo::gbif("Tapirira guianensis")
manimax=mani[,c("species","lon","lat", "municipality", "adm1")]
manimax=na.exclude(manimax)

pts = manimax

head(pts)


#testando as funções

?rm_accent
?filt

rm_accent(pts$municipality)

filt(manimax)




library(rgdal)
library(dismo)

#br_mun = readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp", encoding = "UTF-8")
#br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

use_data(br_mun, overwrite = T)

