#' @title Marks occurrences based on user supplied vector file
#' @name filt.generic
#'
#' @description A function to mark occurrences based on user supplied vector file.
#'
#' @param pts data.frame. Table with points of occurrence.
#' @param shape 'SpatialPolygonsDataFrame' object with the attribute to be checked.
#' @param few.pts logical. If TRUE (default), removes species with few records.
#' @param value minimum number of records to keep the species.
#' @param plot logical. If TRUE (default), the points will be plotted on the provided vector object
#' @param file output filename
#'
#' @details empty yet
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#'
#' filt.generic(Eugenia_aurata, shape = world[world$LONG_NAME == 'Brazil',])
#'
#' @import rgdal
#' @import sp
#' @importFrom graphics legend
#' @importFrom stats na.exclude
#' @importFrom utils head write.table
#'
#' @export

filt.generic = function(pts,
                        shape = NULL,
                        few.pts = F,
                        value = 10,
                        plot = TRUE,
                        file = NULL) {
  if (class(pts) != "data.frame") {
    stop(
      'The occurrence points are only accepted in data.frame format, with at least the columns "sp", "lon", "lat" in the first positions'
    )
  } else{
    st = names(pts)[1] != "sp" |
      names(pts)[2] != "lon" | names(pts)[3] != "lat"
    if (st) {
      stop(
        'The data.frame with occurrence points needs at least the columns "sp", "lon", "lat" in the first positions'
      )
    }
  }
  pts$sp = as.character(pts$sp)
  especies <- unique(pts$sp)
  message(paste0("Species number: ", length(especies)),
          "\n",
          paste0("Records number: ", dim(pts)[1]))
  
  if (few.pts == T) {
    message("\n#   Removing the species with ", value, " or fewer records   #\n")
    pts = remove.pts(pts, especies, value = value)
  }
  
  if (!is.null(shape) &
      class(shape) == "SpatialPolygonsDataFrame") {
    proj4string(shape) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  } else{
    stop("Please provide an object of class SpatialPolygonsDataFrame")
  }
  
  
  message("\n# Checking ... #\n")
  
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
  
  message("\n# Checking the points #\n")
  
  pb <- txtProgressBar(min = 1,
                       max = dim(pts1)[1],
                       style = 3)
  
  #início do filtro
  
  (ini = Sys.time())
  for (i in 1:dim(pts1)[1]) {
    setTxtProgressBar(pb, i)
    
    if (is.na(pts1$loc[i]) & pts1$lon[i] > 90 | pts1$lon[i] < -90) {
      pts1$status[i] = "suspicious"
    }
    
    if (!is.na(pts1$loc[i])) {
      pts1[i, "status"] = "OK"
    }
    
    if (is.na(pts1$status[i])) {
      new = pts1[i,]
      coordinates(new) = ~ lon + lat
      proj4string(new) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result = over(new, am)[, "nome"]
      if (is.na(result)) {
        #testando invertidas
        new1 = pts1[i,]
        coordinates(new1) = ~ lat + lon
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if (!is.na(result)) {
          pts1$status[i] = "inverted"
          pts1[i, c(1, 3, 2)] = pts1[i, c(1, 2, 3)]
        }
      }
      
      if (is.na(pts1$status[i])) {
        #testando sinal lon
        pts1[i, c(2)] = (pts1[i, c(2)]) * -1
        new1 = pts1[i,]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if (!is.na(result)) {
          pts1$status[i] = "lon_signal"
        } else{
          pts1[i, c(2)] = (pts1[i, c(2)]) * -1
        }
      }
      
      if (is.na(pts1$status[i])) {
        #testando sinal lat
        pts1[i, c(3)] = (pts1[i, c(3)]) * -1
        new1 = pts1[i,]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if (!is.na(result)) {
          pts1$status[i] = "lat_signal"
        } else{
          pts1[i, c(3)] = (pts1[i, c(3)]) * -1
        }
      }
      
      if (is.na(pts1$status[i])) {
        #testando sinal lon e lat
        pts1[i, c(2, 3)] = (pts1[i, c(2, 3)]) * -1
        new1 = pts1[i,]
        coordinates(new1) = ~ lon + lat
        proj4string(new1) <-
          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        result = over(new1, am)[, "nome"]
        if (!is.na(result)) {
          pts1$status[i] = "lon_lat_signal"
        } else{
          pts1[i, c(2, 3)] = (pts1[i, c(2, 3)]) * -1
        }
      }
      
      if (is.na(pts1$status[i])) {
        #testando sinal lon e lat
        pts1$status[i] = "suspicious"
      }
      
    }
    
  }
  
  fim = Sys.time()
  message("\n Finished: ", round(fim - ini, 1))
  
  pts1$status = as.factor(pts1$status)
  print(table(pts1$status))
  
  if (plot == T) {
    pts = pts1
    coordinates(pts) = ~ lon + lat
    plot(am, axes = T, border = "gray75")
    points(pts, col = pts1$status, pch = "+")
    #legend("bottomright", legend = unique(pts1$status), pch = 16,col = unique(pts1$status), bty="n", bg = "white")
    legend(
      "bottomright",
      legend = unique(pts1$status),
      pch = "+",
      col = unique(pts1$status),
      bty = "o",
      bg = "white"
    )
  }
  
  especies <- unique(pts1$sp)
  #numero de especies
  message(paste0("Species number: ", length(especies)),
          "\n",
          paste0("Records number: ", dim(pts1)[1]))
  
  if (!is.null(file)) {
    write.table(pts1, file, sep = ";", row.names = F)
  }
  invisible(pts1)
}
