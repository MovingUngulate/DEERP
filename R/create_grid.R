#' @title Creation of Common Raster Grid
#
#' @description Create a common raster grid for prediction
#' @param xmin min x coordinate
#' @param xmax max x coordinate
#' @param ymin min y coordinate
#' @param ymax max y coordinate
#' @param res cell size resolution
#' @param projstring proj4string for raster
#' @return Returns a raster for use in other functions
#' @keywords capture, animal ID, homerange
#' @export
#' @examples
#' \donttest{hr<-create_grid(xmin = 620000, xmax = 702000, ymin = 4510000, ymax = 4620000, res = 10, projstring = '+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')}

create_grid<-function(xmin, xmax, ymin, ymax, res, projstring){
  x <- seq(xmin,xmax,by=res) # where resolution is the pixel size you desire 
  y <- seq(ymin,ymax,by=res)
  xy <- expand.grid(x=x,y=y)
  sp::coordinates(xy) <- ~x+y
  sp::gridded(xy) <- TRUE

  sp::proj4string(xy)<-projstring
  
  return(xy)
}