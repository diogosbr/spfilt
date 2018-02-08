#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species","lon","lat", "municipality", "adm1"
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
#' #Obtendo coordenadas da espécie/grupo de interesse
#' mani=dismo::gbif("Manilkara maxima")
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

filt = function(pts, shape.municipios = NULL) {
  if (class(pts) != "data.frame" & class(pts) != "matrix") {
    stop("Invalid format. Please enter 'data.frame' or 'matrix'.")
  }
  
  if (dim(pts)[2] <= 4) {
    stop(
      "The 'pts' argument must have three columns: 'species', 'lon', 'lat', 'municipality', 'UF'"
    )
  }
  
  pts = na.exclude(pts)
  
  coordinates(pts) <- ~ lon + lat
  
  if (is.null(shape.municipios)) {
    br_mun
  }
  if (is.null(shape.municipios) == FALSE &
      class(shape.municipios) == "SpatialPolygonsDataFrame") {
    br_mun = shape.municipios
    proj4string(br_mun) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  }
  
  proj4string(pts) <-
    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  pts1 = as.data.frame(pts)
  
  
  muni_shape = over(pts, br_mun)[, c('NOMEMUNICP', 'NOMEUF')]
  muni_shape[, 1] = as.vector(muni_shape[, 1])
  muni_shape[, 2] = as.vector(muni_shape[, 2])
  pts1 = cbind(pts1, muni_shape)
  pts1[, 4] = as.vector(pts1[, 4])
  pts1[, 5] = as.vector(pts1[, 5])
  
  for (i in 4:dim(pts1)[2]) {
    pts1[, i] = tolower(pts1[, i])
    pts1[, i] = tolower(pts1[, i])
  }
  pts1$NOMEMUNICP = rm_accent(pts1$NOMEMUNICP)
  pts1$municipality = rm_accent(pts1$municipality)
  pts1$filt = "Ok"
  
  for (i in 1:dim(pts1)[1]) {
    if (is.na(pts1$municipality == pts1$NOMEMUNICP)[i] == TRUE) {
      pts1[i, "filt"] = "outside Brazil"
      pts1[i, "NOMEMUNICP"] = "outside Brazil"
      pts1[i, "NOMEUF"] = "outside Brazil"
    }
    if ((pts1$municipality == pts1$NOMEMUNICP)[i] == FALSE) {
      pts1[i, "filt"] = "outside county"
    }
    if ((pts1[i, "NOMEMUNICP"] == "outside Brazil")) {
      pts1[i, "filt"] = "outside Brazil"
      pts1[i, "NOMEMUNICP"] = "not found"
      pts1[i, "NOMEUF"] = "not found"
    }
  }
  
  pts2 = pts1[, -c(5, 7)]
  names(pts2) = c("species",
                  "lon",
                  "lat",
                  "county.orig" ,
                  "county.shape",
                  "status")
  
  print(table(pts2$status))
  return(pts2)
}