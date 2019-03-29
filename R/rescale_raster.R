#' @title Rescale Raster 0-1
#
#' @description Rescale a raster from 0 to 1
#' @param x raster containing values to rescale
#' @return rescaled raster
#' @keywords capture, animal ID, homerange
#' @export
#' @examples
#' \donttest{dat<-rescale_raster(x = ras)}


rescale_raster<-function(x){
  ((x-cellStats(x,"min"))/(cellStats(x,"max")-cellStats(x,"min")))
}