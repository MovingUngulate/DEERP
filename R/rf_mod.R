#' @title RF RSF-type models
#
#' @description Create an RF RSF type model
#' @param x output of rf_sampling
#' @param ras raster stack of covars
#' @param basepath base location to save to
#' @param ntree number of trees to create in RF Model
#' @param return logical, if TRUE, returns the predicted raster
#' @return Predict spatial rf models
#' @keywords sampling, rf
#' @export
#' @examples
#' \donttest{system.time({ sampout<-lapply(d[1],rf_sampling,dat=df,nsamps=2,nt=5,maxnt=5,proj=dfproj,ras=nras) })}


rf_mod<-function(x, ras, basepath, ntree){
  
  pts <- x[[1]]
  
  tab <- x[[2]]
  
  outn<-paste0(basepath,'RF_',tab$Species,'_',tab$Year,'_',tab$Month,'.tif')
  
  ext<-raster::extract(ras,pts,df=TRUE)
  
  spdf<-as.data.frame(pts)
  
  spdf<-cbind(spdf,ext)
  spdf<-spdf[complete.cases(spdf[,8:17]),]
  train<-data.frame()
  test<-data.frame()
  for(i in 0:1){
    st<-spdf[spdf$Used==i,]
    st$N<-1:nrow(st)
    
    st_train<-st[sample(nrow(st)*.9),]
    st_test<-st[!(st$N %in% st_train$N),]
    
    train<-rbind(train,st_train)
    test<-rbind(test,st_test)
  }
  
  rfmod<-randomForest::randomForest(x=train[,8:17],y=as.factor(train$Used),ntree=ntree)
  
  test$Pred<-predict(rfmod,test[,8:17],type='prob')[,2]
  
  pred<-raster::predict(ras,rfmod,progress='text',type='prob')
  pred<-abs(pred-1)
  raster::writeRaster(pred,filename=outn)
  return(test)
  
}