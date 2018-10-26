#' @title Removing species with few records.
#' @name remove.pts
#'
#' @description A function to remove species with few records.
#'
#' @param pts data.frame. Table with points of occurrence, including the municipalities informed on the label. the data frame must contain the following columns in this order: "species","lon","lat", "municipality", "adm1"
#' @param especies vector. 
#' @param value Minimum number of records per species.
#'
#' @details
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#' 
#' remove.pts(euterpe, unique(euterpe$sp))
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' 
#' @export
remove.pts = function(pts, especies, value = 10){
  if(length(especies)>1){
    pb <- txtProgressBar(min = 1,
                         max = length(especies),
                         style = 3)
  }
  registros.10 = c()
  for (especie in especies) {
    if(length(especies)>1){
      setTxtProgressBar(pb, grep(especie, especies)[1])
    }
    occs <- pts[pts$sp == especie,]
    if (dim(occs)[1] >= value) {
      registros.10 = rbind(registros.10, occs)
    }
  }
  if(is.null(registros.10)){
    message("No species has more than ", value, " records")
  }else{return(registros.10)}
}
