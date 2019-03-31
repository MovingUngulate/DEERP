#' @title Spatial Sampling
#
#' @description Create random sample in proximity to used samples
#' @param x list of data.frames containing 'Month','Year','Spp'
#' @param dat data.frame of relocation data
#' @param nsamps multiplier, number of used locations times nsamps = number available locations
#' @param nt mean step length times nt = minimum distance to sample from
#' @param maxnt mean step length times nt times maxnt = maximum distance to sample from
#' @param proj proj4string of point data
#' @param ras raster grid to sample from
#' @return Returns a list containing a report on the sampling and the used/available samples
#' @keywords sampling, rf
#' @export
#' @examples
#' \donttest{system.time({ sampout<-lapply(d[1],rf_sampling,dat=df,nsamps=2,nt=5,maxnt=5,proj=dfproj,ras=nras) })}



rf_sampling<-function(x, dat, nsamps, nt, maxnt, proj, ras){
  
  subd<-dat[dat$Spp==x$Species&dat$Month==x$Month&dat$Year==x$Year,]
  
  
  coordinates(subd)<-~Easting+Northing
  proj4string(subd)<-proj
  
  tes<-raster::rasterize(subd,ras,field=1)
  dist=raster::distance(tes)
  
  bufmindist<-mean(subd@data$dist,na.rm=T)*nt
  bufmaxdist<-bufmindist*maxnt
  
  dist[dist>bufmaxdist]<-NA
  dist[dist<bufmindist]<-NA
  
  dist[dist>1]<-1
  
  samp<-raster::sampleRandom(dist,size=(nrow(subd@data)*nsamps), sp=TRUE)
  
  x$MeanStepLength<-mean(subd@data$dist,na.rm=T)
  x$MinDist<-bufmindist
  x$MaxDist<-bufmaxdist
  x$NSamp<-nsamps
  x$NUsed<-nrow(subd@data)
  x$NAvail<-nrow(samp@data)
  
  subd@data$Used<-1
  samp@data$Used<-0
  samp@data$Month<-subd@data$Month[1]
  samp@data$Year<-subd@data$Year[1]
  samp@data$Spp<-subd@data$Spp[1]
  
  all<-rbind(samp[,c('Used','Month','Year','Spp')],subd[,c('Used','Month','Year','Spp')])
  
  return(list(all,x))
  
}