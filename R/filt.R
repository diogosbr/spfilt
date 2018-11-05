#' @title Mark occurrences with the name informed different from the coordinate in a user supplied vector file
#' @name filt
#'
#' @description A function to check whether the coordinates are consistent with the name of the polygon.
#'
#' @param pts Data.frame. Table with points of occurrence, including the municipalities informed on the label.
#' @param shape It can be a shape of municipalities of Brazil in format "SpatialPolygonsDataFrame".
#' @param field.pts String. Name of pts column containing the standard to be searched for.
#' @param field.shape String.Name of shape column containing the standard to be searched for.
#'
#' @details The initial idea of this function is to verify if the coordinates informed in the label of the registries are located within the limits of the municipality informed by the label. Returns a table equal to the one entered by adding a column (status) indicating whether the coordinates are consistent.
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#'
#' eug <- filt(Eugenia_aurata, shape = world, field.pts = "country", field.shape = "LONG_NAME")
#' eug
#'
#' @import dismo
#' @import maptools
#' @import rgdal
#' @import sp
#' @import textclean
#' @importFrom graphics legend
#' @importFrom stats na.exclude
#' @importFrom utils head write.table
#'
#' @export

filt  <-  function(pts,
                shape = NULL,
                field.pts = "municipality",
                field.shape = "NOMEMUNICP") {
  if (class(pts) != "data.frame" & class(pts) != "matrix") {
    stop("Invalid format. Please enter 'data.frame' or 'matrix'.")
  }
  if (length(pts[, 'lon']) != length(na.exclude(pts[, 'lon']))) {
    stop(
      "Sorry, but this function does not yet accept NA in the field longitude or latitude. Remove NA's and try again."
    )
  }
  
  if (length(table(pts[, "lat"] > 90)) >= 2 |
      length(table(pts[, "lat"] < (-90))) >= 2) {
    stop("There is no latitude greater than 90 or less than -90")
  }
  
  if (length(table(pts[, "lon"] > 180)) >= 2 |
      length(table(pts[, "lon"] < (-180))) >= 2) {
    stop("There is no longititude greater than 180 or less than -180")
  }
  
  coordinates(pts) <- ~ lon + lat
  
  if (is.null(shape)) {
    #shape  <-  world
    #field.shape  <-  "LONG_NAME"
    stop("Please provide a valid vector file")
  }
  
  if (!is.null(shape) &
      class(shape) == "SpatialPolygonsDataFrame") {
    shape <- spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  } else{
    stop("Please provide an object of class SpatialPolygonsDataFrame")
  }
  
  proj4string(pts) <-
    CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  pts1 <-  as.data.frame(pts)
  
  from_shape  <-  over(pts, shape)[, field.shape]
  from_shape  <-  as.vector(from_shape)
  pts1  <-  cbind(pts1, from_shape)
  
  pts1[, field.pts] <-  as.vector(pts1[, field.pts])
  pts1[, "from_shape"] <-  as.vector(pts1[, "from_shape"])
  
  pts1[, field.pts]  <-  tolower(pts1[, field.pts])
  pts1[, "from_shape"]  <-  tolower(pts1[, "from_shape"])
  
  pts1[, field.pts]  <-  textclean::replace_non_ascii(pts1[, field.pts])
  pts1[, "from_shape"]  <-  textclean::replace_non_ascii(pts1[, "from_shape"])
  
  pts1$status  <-  NA
  
  message("Processing. This may take a few minutes.\n")
  
  message("Step 1 ...\n")
  for (i in 1:dim(pts1)[1]) {
    if (is.na(pts1[i, field.pts])) {
      pts1[i, "status"]  <-  paste0("original ", field.pts, " empty")
    }
    if (is.na(pts1[i, "from_shape"])) {
      pts1[i, "status"]  <-  "outside the polygon "
    }
    if (is.na(pts1[i, 'status'])) {
      if (pts1[i, field.pts] == pts1[i, 'from_shape']) {
        pts1[i, 'status']  <-  "OK"
      } else{
        pts1[i, 'status']  <-  "suspicious"
      }
    }
  }
  
  message("Step 2 ...\n")
  #invert lon lat
  for (i in 1:dim(pts1)[1]) {
    if (pts1[i, 'status'] == "suspicious" | pts1[i, "status"] == "outside the polygon ") {
      new1  <-  pts1[i,]
      coordinates(new1)  <-  ~ lat + lon
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result  <-  over(new1, shape)[, field.shape]
      result  <-  tolower(result)
      new2  <-  data.frame(new1)
      if (!is.na(result)) {
        if (new2[, field.pts] == result) {
          pts1$status[i]  <-  "inverted"
          pts1[i, c('lat', 'lon')]  <-  pts1[i, c('lon', 'lat')]
        } else{
          pts1[i, 'status']  <-  "suspicious"
        }
      }
    }
  }
  
  message("Step 3 ...\n")
  #lon signal
  for (i in 1:dim(pts1)[1]) {
    if (pts1[i, 'status'] == "suspicious"| pts1[i, "status"] == "outside the polygon ") {
      new1  <-  pts1[i, ]
      new1[, c('lon')]  <-  (new1[, c('lon')]) * -1
      coordinates(new1)  <-  ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result  <-  over(new1, shape)[, field.shape]
      result  <-  tolower(result)
      new2  <-  data.frame(new1)
      if (!is.na(result)) {
        if (new2[, field.pts] == result) {
          pts1$status[i] = "lon_signal"
          pts1[i, c('lon')]  <-  (pts1[i, c('lon')]) * -1
        } else{
          pts1[i, 'status']  <-  "suspicious"
        }
      }
    }
  }
  
  message("Step 4 ...\n")
  #lat signal
  for (i in 1:dim(pts1)[1]) {
    if (pts1[i, 'status'] == "suspicious"| pts1[i, "status"] == "outside the polygon ") {
      new1  <-  pts1[i,]
      new1[, c('lat')]  <-  (new1[, c('lat')]) * -1
      coordinates(new1)  <-  ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result  <-  over(new1, shape)[, field.shape]
      result  <-  tolower(result)
      new2  <-  data.frame(new1)
      if (!is.na(result)) {
        if (new2[, field.pts] == result) {
          pts1$status[i]  <-  "lat_signal"
          pts1[i, c('lat')]  <-  (pts1[i, c('lat')]) * -1
        } else{
          pts1[i, 'status']  <-  "suspicious"
        }
      }
    }
  }
  
  message("Step 5 ...\n")
  #lon lat signal
  #lat signal
  for (i in 1:dim(pts1)[1]) {
    if (pts1[i, 'status'] == "suspicious"| pts1[i, "status"] == "outside the polygon ") {
      new1  <-  pts1[i, ]
      new1[, c('lon', 'lat')]  <-  (new1[, c('lon', 'lat')]) * -1
      coordinates(new1)  <-  ~ lon + lat
      proj4string(new1) <-
        CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      result  <-  over(new1, shape)[, field.shape]
      result  <-  tolower(result)
      new2  <-  data.frame(new1)
      if (!is.na(result)) {
        if (new2[, field.pts] == result) {
          pts1$status[i] = "lon_lat_signal"
          pts1[i, c('lon', 'lat')]  <-  (pts1[i, c('lon', 'lat')]) * -1
        } else{
          pts1[i, 'status']  <-  "suspicious"
        }
      }
    }
  }
  
  message("Finished ...\n")
  print(table(pts1$status))
  invisible(pts1)
}
