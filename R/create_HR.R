#' @title Create homerange for an individual
#
#' @description Create a kernel UD HR for an individual using a custom grid
#' @param dat data.frame with relocation data
#' @param grid grid as output by DEERP::create_grid
#' @param aidname name of aid column
#' @return Returns a raster HR for an individual on a common grid
#' @keywords capture, animal ID, homerange
#' @export
#' @examples
#' \donttest{hr<-HRFun(dat = dat, grid = grid)}

create_HR<-function(dat,grid, aidname){
  dat@data[,aidname]<-as.character(dat@data[,aidname])
  tab<-as.data.frame(table(dat@data[,aidname]))
  tab<-tab[tab$Freq>50,]
  dat@data<-dat@data[dat@data[,aidname] %in% tab$Var1,]
  
  dat@data[,aidname]<-as.factor(dat@data[,aidname])
  
  system.time({ kern<-adehabitatHR::kernelUD(dat[,aidname],grid=grid) })
  
  system.time({ hr<-adehabitatHR::estUDm2spixdf(kern) })
  
  nr<-ncol(hr@data)
  
  hr@data$All<-raster::rowSums(hr@data[,1:nr])
  
  hr1<-raster::raster(hr,layer='All')
  
  
  names(hr1)<-paste(dat$Spp[1],dat$Month[1],dat$Year[1],sep='_')
  
  return(hr1)
  
}
