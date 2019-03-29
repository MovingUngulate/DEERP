#' @title clean all data
#
#' @description Download new and historic data and bind all data together
#' @param usernames vector of ATS usernames
#' @param passwords vector of ATS passwords
#' @param ST TRUE/FALSE whether to get sirtrack data
#' @param STUser email address for sirtrack
#' @param STPass password for sirtrack
#' @param tempdir Temp directory
#' @param lookup capture history table
#' @param histdata historic shapefile path
#' @param vecpath file path to location of vectronic data
#' @return Resulting object is a list of Two elements. First element is a SpatialPointsDataFrame of
#' all the GPS data on the account, the second is a dataframe of all transmissions received by
#' the satellite
#'
#' \strong{Spatial Data Column Description:}
#' \item{CollarSerialNumber}{ATS Designated Collar Serial Number}
#' \item{TelemDate}{POSIX Field of collare fixes in the USER'S timezone. NOTE: may need to alter timezone}
#' \item{HDOP}{Horizontal Dilution of Precision}
#' \item{NumSats}{Number of satellites used for GPS Fix}
#' \item{2D/3D}{Whether fix is a 2d or 3d fix. Values are either 2 or 3}
#' @keywords Iridium, iridium
#' @export
#' @examples
#' \donttest{gather_data(usernames=c('yourusername'),passwords=c('yourpassword'),ST=TRUE,STUser='stusername',STPass='stpassword',tempdir='/home/mhayes1/Desktop/Testing/',lookup='path/to/db.csv',histdata='path/to/hist.shp',vecpath='/path/to/vec/data/')}
#'

gather_data<-function(usernames,
                      passwords,
                      ST,
                      STUser,
                      STPass,
                      tempdir,
                      lookup,
                      histdata,
                      vecpath){
  
  histdat<-rgdal::readOGR(histdata,verbose=FALSE)
  
  gps <- Part::CombDat(vecpath= vecpath,
                       ATSUsers = usernames,
                       ATSPass = passwords,
                       tempdir = tempdir,
                       ST=ST,
                       STUser= STUser,
                       STPass= STPass)
  
  
  names(histdat@data)<-names(gps@data)
  histdat@data$CollarSerialNumber<-as.character(histdat@data$CollarSerialNumber)
  histdat@data$TelemDate<-as.POSIXct(as.character(histdat@data$TelemDate),'%Y-%m-%d %H:%M:%S', tz='GMT')
  histdat@data$HDOP<-as.numeric(histdat@data$HDOP)
  histdat@data$X2D.3D<-as.integer(histdat@data$X2D.3D)
  histdat@data$Temperature<-as.integer(histdat@data$Temperature)
  
  gps<-rbind(histdat,gps)
  
  gps$chk<-paste(gps$CollarSerialNumber,gps$TelemDate,sep='_')
  gps<-gps[!duplicated(gps$chk),]
  
  gps$Date<-as.Date(gps$TelemDate, format = "%Y-%m-%d", tz = "MST")
  
  cap<-read.csv(lookup, stringsAsFactors = F)
  
  retCapHist<-function(lk){
    x<-DEERP::collar_history(capdat = lk, df = '%m/%d/%Y')
    
    x$Ser1Start<-as.Date(x$Ser1Start, "%Y-%m-%d")
    x$Ser1End<-as.Date(x$Ser1End,"%Y-%m-%d")
    x$Ser2Start<-as.Date(x$Ser2Start,"%Y-%m-%d")
    x$Ser2End<-as.Date(x$Ser2End,"%Y-%m-%d")
    x$Ser3Start<-as.Date(x$Ser3Start,"%Y-%m-%d")
    x$Ser3End<-as.Date(x$Ser3End,"%Y-%m-%d")
    x$Ser4Start<-as.Date(x$Ser4Start,"%Y-%m-%d")
    x$Ser4End<-as.Date(x$Ser4End,"%Y-%m-%d")
    
    return(x)
  }
  
  x<-retCapHist(lookup)
  
  for(i in 1:nrow(x)){
    xx<-x[i,]
    
    for(l in 1:4){
      if(l == 1){
        xxx<-xx[,1:4]
      }
      if(l == 2){
        xxx<-xx[,c(1,5,6,7)]
      }
      if(l == 3){
        xxx<-xx[,c(1,8:10)]
      }
      if(l == 4){
        xxx<-xx[,c(1,11:13)]
      }
      
      if(is.na(xxx[1,2])){next}
      ss<-gps[gps$CollarSerialNumber==xxx[1,2],]
      ss<-ss[(ss$Date>=xxx[,3])&ss$Date<=(xxx[,4]),]
      ss<-ss[complete.cases(ss$Latitude),]
      
      c<-cap[cap$UAID == xxx[1,1],]
      
      #ss<-as.data.frame(ss)
      if(nrow(ss)==0){next}
      if(nrow(ss)>0){
        ss$AID<-xxx[1,1]
        ss$Sex<- c$Sex[1]
        ss$Spp<-c$Species[1]
      }
      
      if(l == 1){
        ald<-ss
      }
      if(l>1){
        ald<-rbind(ald,ss)
      }
      
    }
    if(i == 1){
      outsp<-ald
    }
    if(i>1){
      outsp<-rbind(outsp,ald)
    }
  }
  
  proj<-sp::proj4string(outsp)
  outsp<-as.data.frame(outsp)
  outsp<-outsp[!duplicated(outsp[,1:4]),]
  outsp<-outsp[complete.cases(outsp$CollarSerialNumber),]
  
  sp::coordinates(outsp)<-~Longitude+Latitude
  sp::proj4string(outsp)<-proj
  
  return(outsp2)
  
}