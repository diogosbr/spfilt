#' @title Mark occurrences with the municipality informed different from the coordinate
#' @name filt
#'
#' @description A function to mark occurrences with the municipality informed different from the coordinate.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species","lon","lat", "municipality", "adm1"
#' @param inverted logical. If TRUE (default), it will check if longitude and latitude are changed. For now this option may be slow when there are many records of occurrence.
#' @param shape.municipios It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame". If it is NULL, the Brazilian shape will be used available on the IBGE website.
#'
#' @details
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#' 
#' filt.generic(euterpe)
#'
#' @import raster
#' @import rgdal
#' @import sp
#' @import utils
#' 
#' @export

filt.generic = function(pts, shape, few.pts = T, value = 10, plot = TRUE, file = NULL){
  if(class(pts)!="data.frame"){
    stop('The occurrence points are only accepted in data.frame format, with at least the columns "sp", "lon", "lat" in the first positions')
  }else{
    st = names(pts)[1]!="sp"|names(pts)[2]!="lon"|names(pts)[3]!= "lat"
    if(st){stop('The data.frame with occurrence points needs at least the columns "sp", "lon", "lat" in the first positions')}}
  pts$sp = as.character(pts$sp)
  especies <- unique(pts$sp)
  message(paste0("Species number: ", length(especies)), "\n", paste0("Records number: ", dim(pts)[1]))
  
  if(few.pts == T){
    message("\n#   Removing the species with", value,"or fewer records   #\n")
    pts = remove.pts(pts, especies, value = value)
  }
  
  message("\n# Checking the points #\n")
  
  coordinates(pts) <- ~ lon + lat
  
  proj4string(pts) <-
    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  pts1 = as.data.frame(pts)
  
  am = shape
  am = spTransform(am,
                   CRS(
                     "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
                   ))
  am$nome = "Neotrop"
  pres = over(pts, am)[, "nome"]
  
  
  pts1$loc = pres
  head(pts1)
  
  pts1$status = NA
  head(pts1)
  
  
  pb <- txtProgressBar(min = 1,
                       max = dim(pts1)[1],
                       style = 3)
  
  #início do filtro
  
  (ini = Sys.time())
  for (i in 1:dim(pts1)[1]) {
    
    setTxtProgressBar(pb, i)
    
    if (is.na(pts1$loc[i]) & pts1$lon[i] > 90 | pts1$lon[i] < -90) {
      pts1$status[i] = "suspeita"
    }
    
    if(!is.na(pts1$loc[i])){
      pts1[i,"status"]="OK"
    }
    
    if (is.na(pts1$status[i])) {
      new = pts1[i, ]
      coordinates(new) = ~ lon + lat
      proj4string(new) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new, am)[, "nome"]
      if(is.na(result)){#testando invertidas
        new1 = pts1[i, ]
        coordinates(new1) = ~ lat + lon
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if(!is.na(result)){
          pts1$status[i] = "invertida"
          pts1[i, c(1, 3, 2)] = pts1[i, c(1, 2, 3)]
        }
      }
      
      if(is.na(pts1$status[i])){#testando sinal lon
        pts1[i, c(2)] = (pts1[i, c(2)])*-1
        new1 = pts1[i, ]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if(!is.na(result)){
          pts1$status[i] = "sinal_lon"
        }else{pts1[i, c(2)] = (pts1[i, c(2)])*-1}
      }
      
      if(is.na(pts1$status[i])){#testando sinal lat
        pts1[i, c(3)] = (pts1[i, c(3)])*-1
        new1 = pts1[i, ]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if(!is.na(result)){
          pts1$status[i] = "sinal_lat"
        }else{pts1[i, c(3)] = (pts1[i, c(3)])*-1}
      }
      
      if(is.na(pts1$status[i])){#testando sinal lon e lat
        pts1[i, c(2,3)] = (pts1[i, c(2,3)])*-1
        new1 = pts1[i, ]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if(!is.na(result)){
          pts1$status[i] = "sinal_lon_lat"
        }else{pts1[i, c(2,3)] = (pts1[i, c(2,3)])*-1}
      }
      
      if(is.na(pts1$status[i])){#testando sinal lon e lat
        pts1$status[i] = "suspeita"
      }
      
    }
    
  }
  
  fim = Sys.time()
  message("\n Finished: ")
  message(round(fim - ini))
  
  pts1$status = as.factor(pts1$status)
  print(table(pts1$status))
  
  if(plot == T){
    pts = pts1
    coordinates(pts) = ~lon+lat
    plot(am, axes = T)
    points(pts, col = pts1$status, pch = "+")
    #legend("bottomright", legend = unique(pts1$status), pch = 16,col = unique(pts1$status), bty="n", bg = "white")
    legend("bottomright", legend = unique(pts1$status), pch="+", col = unique(pts1$status), bty="o", bg = "white")
  }
  
  
  especies <- unique(pts1$sp)
  #número de espécies
  message("Numero de especies: ", length(especies))
  
  if(!is.null(file)){
    write.table(pts1, file, sep = ";", row.names = F)
  }
  return(pts1)
}
