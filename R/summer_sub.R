#' @title Monthly summer subset
#
#' @description Subset data by month/year for summer months (6, 7, 8)
#' @param x data.frame containing relocation data
#' @param spp text string denoting species
#' @return Returns summer subset data
#' @keywords capture, animal ID, homerange
#' @export
#' @examples
#' \donttest{dat<-summer_sub(x = dat, spp='FMD')}


summer_sub<-function(x,spp){
  
  x$Month<-strftime(x$TelemDate,format='%m')
  x$Year<-strftime(x$TelemDate,format='%Y')
  
  x$Spp<-spp
  
  
  return(list(x[x$Month=='06'&x$Year=='2016',],
              x[x$Month=='07'&x$Year=='2016',],
              x[x$Month=='08'&x$Year=='2016',],
              x[x$Month=='06'&x$Year=='2017',],
              x[x$Month=='07'&x$Year=='2017',],
              x[x$Month=='08'&x$Year=='2017',],
              x[x$Month=='06'&x$Year=='2018',],
              x[x$Month=='07'&x$Year=='2018',],
              x[x$Month=='08'&x$Year=='2018',]))
  
}