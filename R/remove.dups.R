#' @title Remove duplicated and NAs
#' @name remove.dups
#'
#' @description A function to check whether the coordinates are consistent with the name of the polygon.
#'
#' @param pts Data.frame. Table with points of occurrence, including the municipalities informed on the label.
#' @param na.rm String. Name of pts column containing the standard to be searched for.
#' @param mask String.Name of shape column containing the standard to be searched for.
#'
#' @details The
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#'
#' eug <- remove.dups(Eugenia_aurata, mask = example, na.rm = FALSE)
#' eug
#'
#' @importFrom raster cellFromXY extract
#'
#' @export

remove.dups  <-  function(pts,
                          mask = example,
                          na.rm = FALSE,
                          dup = T) {
  if (names(pts)[1] == "sp"  |
      names(pts)[1] == "lon" | names(pts)[1] == "lat") {
    species <- unique(pts$sp)
    
    if (dup == T) {
      message("cleaning duplicates")
      dupls <- !duplicated(pts[, c('sp', 'lon', 'lat')])
      registros <- registros[dupls,]
    }
    
    if (exists("mask")) {
      if (length(species > 1)) {
        pb <- txtProgressBar(min = 1,
                             max = length(species),
                             style = 3)
        registros.unique = c()
        for (i in 1:length(species)) {
          setTxtProgressBar(pb, i)
          # Selecionar pontos espacialmente únicos #
          cell <-
            cellFromXY(mask, pts[pts$sp == species[i], c("lon", "lat")])  # get the cell number for each point
          dup <- duplicated(cell)
          pts2 <-
            pts[pts$sp == species[i] |
                  !dup,]  # select the records that are not duplicated
          registros.unique = rbind(registros.unique, pts2)
        }
        if (na.rm == TRUE) {
          message("\n Removing NA \n")
          pts1 <-
            pts1[!is.na(extract(mask, pts1[, c('lon', 'lat')])),]  #selecionando apenas pontos que tem valor de raster
        }
      } else{
        # Selecionar pontos espacialmente únicos #
        cell <-
          cellFromXY(mask, pts[, c("lon", "lat")])  # get the cell number for each point
        dup <- duplicated(cell)
        pts1 <-
          pts[!dup,]  # select the records that are not duplicated
        
        if (na.rm == TRUE) {
          pts1 <-
            pts1[!is.na(extract(mask, pts1[, c('lon', 'lat')])),]  #selecionando apenas pontos que tem valor de raster
        }
      }
      
      cat(dim(pts)[1] - dim(pts1)[1], "points removed\n")
      cat(dim(pts1)[1], "spatially unique points\n")
    } else
      (cat("Indicate the object with the predictive variables"))
  } else{
    stop("The first three columns should be 'sp', 'lon' and 'lat' necessarily in this order")
  }
  
  message("Finished ...\n")
  if (exists(pts1)) {
    invisible(pts1)
  }
  if (!is.null(registros.unique)) {
    invisible(registros.unique)
  }
  
}
