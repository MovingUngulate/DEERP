#' @title Sampling for SSF Modeling
#
#' @description Create dataset for ssf modeling
#' @param x list of data.frames with Month, Year and Species columns
#' @param dat dataframe of all data to subset
#' @param ras stack of rasters to extract to points
#' @param proj projection of point data
#' @param basepath base location to RF rasters
#' @param nran number of random points for each used
#' @return Model ready DF
#' @keywords sampling, ssf
#' @export
#' @importFrom dplyr "%>%"
#' @examples
#' \donttest{system.time({ od<-lapply(d[1],ssf_sample, dat=df, ras=rasstack, proj=proj4string(rasstack), basepath='/home/puma/Desktop/DEERP/RF/', nran=5) })}



ssf_sample<-function(x, dat, ras, proj, basepath, nran){
  
  subd<-dat[dat$Spp==x$Species&dat$Month==x$Month&dat$Year==x$Year,]
  subd$burst=1
  st<-amt::make_track(subd, .x = Easting, .y = Northing, .t = TelemDate, id = AID, burst_ = burst, crs = sp::CRS(proj))
  
  ssf1 <- st %>% steps_by_burst()
  trk.class<-class(st)
  
  nest.track<-st%>%nest(-id)
  
  st<-st %>% nest(-id) %>%
    mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"),
           dir_rel = map(data, direction_rel),
           sl = map(data, step_lengths),
           nsd_=map(data, nsd))%>%unnest()
  
  st<-st%>%
    mutate(
      week=week(t_),
      month = month(t_, label=TRUE),
      year=year(t_),
      hour = hour(t_)
    )
  
  class(st)<-trk.class
  
  st<-st %>% group_by(id) %>% mutate(dt_ = t_ - lag(t_, default = NA))
  
  
  #### Re sample tracks and append bursts to each id #####
  
  st %>% nest(-id) %>% mutate(sr = map(.$data, summarize_sampling_rate)) %>%
    select(id, sr) %>% unnest()
  
  ssfdat<- st %>% nest(-id) %>%
    mutate(ssf = map(data, function(d){
      d %>%
        track_resample(rate = hours(1), tolerance = minutes(15)) %>%
        filter_min_n_burst(min_n = 3) %>%
        steps_by_burst() %>% random_steps(nran) ## can specify number of random steps desired
    })) %>% select(id, ssf) %>% unnest()
  
  
  ssfdat$utm.easting<-ssfdat$x2_
  ssfdat$utm.northing<-ssfdat$y2_
  
  ssfdat2 <- SpatialPointsDataFrame(ssfdat[,c("x2_","y2_")], ssfdat,
                                    proj4string=CRS(proj))
  ssf.df <- data.frame(spTransform(ssfdat2, proj))
  names(ssf.df)[c(1,14,15)] <-c("AID", "Easting", "Northing")
  ssf.df$timestamp<-ssf.df$t1_
  #ssf.df %>% select(Easting, Northing, x1_, x2_, y1_, y2_) %>% head
  
  ssf.df<-ssf.df[,c(1,19,3,4,12,13,14,15)]
  
  ssf.df$Sex<-subd$Sex[1]
  ssf.df$Spp<-subd$Spp[1]
  ssf.df$Month<-subd$Month[1]
  ssf.df$Year<-subd$Year[1]
  
  targspp<-ifelse(subd$Spp[1]=='MD','ELK','MD')
  
  rasnam<-paste0('RF_',targspp,'_',subd$Year[1],'_',subd$Month[1],'.tif')
  
  raspath<-paste0(basepath,rasnam)
  
  nras<-raster::stack(ras,raster(raspath))
  
  sp::coordinates(ssf.df)<-~Easting+Northing
  sp::proj4string(ssf.df)<-proj
  
  ssf.df@data<-cbind(ssf.df@data,raster::extract(nras,ssf.df,df=T))
  
  return(ssf.df)
  
}