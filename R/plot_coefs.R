#' @title Plot coefficients from SSF models
#
#' @description Plot coefficients from SSF models
#' @param x coefficients from parse_coefs
#' @param plotfolder folder to save plots into
#' @return Returns plots of coefficients
#' @keywords ssf, direct competition, coefficients, plots
#' @export
#' @examples
#' \donttest{lapply(spp,plot_coefs,plotfolder='/home/puma/Desktop/DEERP/plots3/')}


plot_coefs<-function(x,plotfolder){
  
  x$PlotOrder<-ifelse(x$Year=='2016'&x$Month=='6',1,
                      ifelse(x$Year=='2016'&x$Month=='7',2,
                             ifelse(x$Year=='2016'&x$Month=='8',3,
                                    ifelse(x$Year=='2017'&x$Month=='6',4,
                                           ifelse(x$Year=='2017'&x$Month=='7',5,
                                                  ifelse(x$Year=='2017'&x$Month=='8',6,
                                                         ifelse(x$Year=='2018'&x$Month=='6',7,
                                                                ifelse(x$Year=='2018'&x$Month=='7',8,
                                                                       ifelse(x$Year=='2018'&x$Month=='8',9,999)))))))))
  
  x<-x[order(x$PlotOrder),]
  
  if(x$Spp[1]=='MD'){
    x$Var<-apply(x,1, function(x) ifelse(grepl('RF',x['Var']),'ElkUse',x['Var']))
  }else{
    x$Var<-apply(x,1, function(x) ifelse(grepl('RF',x['Var']),'MDUse',x['Var'])) 
  }
  
  targ = x$Spp[1]
  covar = ifelse(targ=='MD','Elk','MD')
  #plotvar = ifelse(targ=='MD','ElkUse','MDUse')
  uni<-unique(x$Var)
  for(i in 1:length(uni)){
    plotvar=uni[i]
    
    outn<-paste0(plotfolder,targ,'_',plotvar,'.jpeg')
    jpeg(filename=outn,width=1010,height=521,quality=100)
    plot(x$PlotOrder[x$Var==plotvar],x$Value[x$Var==plotvar],
         pch=20,col=rep(c('lightgreen','darkgreen','orange'),3),
         cex=2,bty='l',xlab='Month/Year',ylab='Coefficient',main=paste0(targ,' selection relative\nto ',covar,' use','\n',plotvar),xaxt='n')
    
    axis(1,1:9,c('June 2016','July 2016','August 2016','June 2017','July 2017','August 2017','June 2018','July 2018','August 2018'))
    dev.off()
  }
}