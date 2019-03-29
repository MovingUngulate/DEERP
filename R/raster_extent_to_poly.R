#' @title Raster extent to polygon
#
#' @description Convert raster extent to polygon with or without buffer
#' @param x raster containing extent
#' @param buffer amount in coordinate units to buffer the polygon
#' @return spatial polygon object
#' @keywords capture, animal ID, homerange
#' @export
#' @examples
#' \donttest{dat<-raster_extent_to_poly(x = ras, buffer = 0)}


raster_extent_to_poly<-function(x,buffer=0){
  proj<-sp::proj4string(x)
  x<-raster::extent(x)
  p <- as(e, 'SpatialPolygons') 
  sp::proj4string(p)<-proj
  
  p<-rgeos::gBuffer(p,width=buffer)
  
  return(p)
} 
