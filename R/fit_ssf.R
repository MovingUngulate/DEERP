#' @title Fit an SSF Model
#
#' @description Construct an SSF model from data created with ssf_sample
#' @param dat data.frame from ssf_sample
#' @param covars character vector of covars for the model
#' @param lcname name of land cover covariate
#' @param rfnum if not null, the column name of the rf rsf layer
#' @return Returns SSF model object
#' @keywords ssf, direct competition
#' @export
#' @examples
#' \donttest{system.time({ mods<- parallel::mclapply(dd,fit_ssf, covars=c('sl_','ta_','DistToMainStream','DistPrimRoad','DistSecRoad','Elevation','Slope','Aspect','TPI','TRI','Roughness','ClosestDist'),lcname='LCClass_03272019',mc.cores=18) })}


fit_ssf<-function(dat, covars, lcname='LCClass_03272019', rfnum = NULL){
  
  proj<-sp::proj4string(dat)
  dat<-as.data.frame(dat)
  
  dat$Used<-ifelse(dat$case_==TRUE,1,0)
  
  dat[,lcname]<-ifelse(dat[,lcname]==1,'Aspen',
                       ifelse(dat[,lcname]==2,'BareGround',
                              ifelse(dat[,lcname]==3,'Cheatgrass',
                                     ifelse(dat[,lcname]==4,'Conifer',
                                            ifelse(dat[,lcname]==5,'Grass',
                                                   ifelse(dat[,lcname]==6,'Greasewood',
                                                          ifelse(dat[,lcname]==7,'LiveJuniper',
                                                                 ifelse(dat[,lcname]==8,'Riparian',
                                                                        ifelse(dat[,lcname]==9,'Rock',
                                                                               ifelse(dat[,lcname]==10,'Sagebrush',
                                                                                      ifelse(dat[,lcname]==11,'Water','poop')))))))))))
  
  lctab<-as.data.frame(table(dat[,lcname]))
  lctab<-lctab[lctab$Freq>200,]
  
  dat<-dat[dat[,lcname] %in% lctab$Var1,]
  dat[,lcname]<-as.factor(dat[,lcname])
  
  #mcal<-paste0('Used ~ ', names(dat)[17], ' + strata(step_id_) + cluster(AID)')
  if(!is.null(rfnum)){
    mcal<-paste0('Used ~ ', paste(covars,sep="",collapse=' + '), ' + ', names(dat)[rfnum], ' + ',
                 'as.factor(',lcname,')',' + ',
                 'amt::strata(step_id_) + survival::cluster(AID)')
    #'amt::strata(AID)')
  }else{
    mcal<-paste0('Used ~ ', paste(covars,sep="",collapse=' + '), ' + ',
                 'as.factor(',lcname,')',' + ',
                 'amt::strata(step_id_) + survival::cluster(AID)')
    #'amt::strata(AID)') 
  }
  
  dat<-dat[complete.cases(dat),]
  
  mod<-survival::clogit(formula=as.formula(mcal), method='efron',data = dat)
  
  return(list(mod,data.frame(Spp = dat$Spp[1],
                             Year = dat$Year[1],
                             Month= dat$Month[1])))
}
