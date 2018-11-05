#' @title Removing species with few records.
#' @name remove.pts
#'
#' @description A function to remove species with few records.
#'
#' @param pts data.frame. Table with points of occurrence.
#' @param especies vector.
#' @param value Minimum number of records per species.
#'
#' @detailsReturns a table with the species with a record number equal to or greater than that reported.
#'
#' @return a data frame
#'
#' @author Diogo S. B. Rocha
#'
#' @examples
#'
#' remove.pts(Eugenia_aurata, unique(Eugenia_aurata$sp))
#'
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @export
remove.pts = function(pts, especies, value = 10) {
  if (length(especies) > 1) {
    pb <- txtProgressBar(min = 1,
                         max = length(especies),
                         style = 3)
  }
  registros.10 = c()
  for (i in 1:length(especies)) {
    if (length(especies) > 1) {
      setTxtProgressBar(pb, i)
    }
    occs <- pts[pts$sp == especies[i], ]
    if (dim(occs)[1] >= value) {
      registros.10 = rbind(registros.10, occs)
    }
  }
  if (is.null(registros.10)) {
    message("No species has more than ", value, " records")
  } else{
    invisible(registros.10)
  }
}
