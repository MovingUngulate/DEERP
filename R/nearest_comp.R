#' @title Compute distance to nearest potential competitor
#
#' @description Determine the distance to nearest competitor within given timeframe
#' @param x data.frame to calculate distance to
#' @param fd entire dataset that includes animals to calculate distance to
#' @param td difference in time to look for animals, in hours
#' @param ncpu number of cpus to use for calculations
#' @return SSF Model ready data with distance to nearest competitor 
#' @keywords ssf, direct competition, coefficients, plots
#' @export
#' @examples
#' \donttest{system.time({ od<-lapply(dd,nearest_comp, fd=df, td=0.5, ncpu=84) })}

nearest_comp<-function(x, fd, td=0.5,ncpu=84){
  fd<-fd[fd$Sex=='F',]
  x$xuni<-paste0(x$AID,'_',x$step_id_)
  xuni<-unique(x$xuni)
  
  if(x$Spp[1]=='MD'){
    fd<-fd[fd$Spp=='ELK',]
  }else{
    fd<-fd[fd$Spp=='MD',]
  }
  
  xunisp<-split(xuni,seq_along(xuni))
  
  tpp<-function(xuni, x, fd, td){
    sub<-x[x$xuni == xuni,]
    
    fd$difft<-abs(as.numeric(difftime(fd$TelemDate,sub$timestamp[1],units='hours')))
    
    fds<-fd[fd$difft<=td,]
    
    
    sub$ClosestDist<-NA
    for(k in 1:nrow(sub)){
      distdf<-data.frame(dist = as.numeric(rgeos::gDistance(sub[k,],fds, byid=T)),
                         index = 1:length(fds),
                         stringsAsFactors = F)
      distdf<-distdf[complete.cases(distdf),]
      dist<-distdf[distdf$dist==min(distdf$dist,na.rm=T),'dist']
      sub$ClosestDist[k]<-dist
    }
    return(sub)
  }
  
  mods<- do.call("rbind",(parallel::mclapply(xunisp,tpp, x = x, fd = fd, td=td, mc.cores=ncpu)))
  
  return(mods)
}