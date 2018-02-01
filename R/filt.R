#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label.
#' @param shape.municipios It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame". If it is NULL, the Brazilian shape will be used available on the IBGE website.
#' 
#' @details 
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#'
#' @examples
#' #Obtendo coordenadas da esp?cie/grupo de interesse
#' mani=dismo::gbif("Tapirira guianensis")
#' manimax=mani[,c("species","lon","lat", "municipality", "adm1")]
#' manimax=na.exclude(manimax)
#' filt(manimax)
#'
#' @import raster
#' @import dismo
#' @import maptools
#' @import rgdal
#' @import sp
#'
#' @export

filt = function(pts, shape.municipios = NULL){
  
  #pts=manimax[,c("species","lon","lat","municipality", "adm1")]
  pts=na.exclude(pts)
  
  #convertendo em um objeto 'spatial'
  coordinates(pts)<- ~lon+lat
  
  if(is.null(shape.municipios)){
    #br_mun=readOGR("./Shapes/brasil_mun_ibge/brasil_mun_ibge.shp")
    br_mun
  } 
  if(is.null(shape.municipios) == FALSE & class(shape.municipios) == "SpatialPolygonsDataFrame"){
    br_mun=shape.municipios
  }
  
  #atribuinto projeções aos shapes e aos pontos
  #br_mun <- spTransform(br_mun, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  #criando um data frame
  pts1=as.data.frame(pts)
  
  #extraindo dados dos shapes a partir dos pontos
  muni_shape=over(pts,br_mun)[,c('NOMEMUNICP','NOMEUF')]
  muni_shape[,1]=as.vector(muni_shape[,1])
  muni_shape[,2]=as.vector(muni_shape[,2])
  pts1=cbind(pts1,muni_shape)
  pts1[,4]=as.vector(pts1[,4])
  pts1[,5]=as.vector(pts1[,5])
  
  for(i in 4:dim(pts1)[2]){
    pts1[,i]=tolower(pts1[,i])
    pts1[,i]=tolower(pts1[,i])
  }
  pts1$NOMEMUNICP = rm_accent(pts1$NOMEMUNICP)
  pts1$municipality = rm_accent(pts1$municipality)
  pts1$filt ="Ok"
  #pts1$filt = NA
  
  for(i in 1:dim(pts1)[1]){
    if(is.na(pts1$municipality==pts1$NOMEMUNICP)[i]==TRUE){
      pts1[i,"filt"]="outside Brazil"
      pts1[i,"NOMEMUNICP"]="outside Brazil"
      pts1[i,"NOMEUF"]="outside Brazil"
    }
    if((pts1$municipality==pts1$NOMEMUNICP)[i]==FALSE){
      pts1[i,"filt"]="outside county"
    }
    if((pts1[i,"NOMEMUNICP"] == "outside Brazil")){
      pts1[i,"filt"]="outside Brazil"
      pts1[i,"NOMEMUNICP"]="not found"
      pts1[i,"NOMEUF"]="not found"
    }
  }
  
  pts2 = pts1[,-c(5,7)]
  names(pts2) = c("species", "lon", "lat", "county.orig" ,"county.shape", "status")
  
  return(pts2)
}