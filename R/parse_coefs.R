#' @title Parse coefficients from SSF Model
#
#' @description Extract covars for plotting from an SSF Model
#' @param x SSF model object
#' @param type character, either RSF or Dist for model type
#' @return Returns Coefficients from SSF model object
#' @keywords ssf, direct competition, coefficients
#' @export
#' @examples
#' \donttest{coef<-as.data.frame(data.table::rbindlist(lapply(mods,parse_coefs)))}


parse_coefs<-function(x, type){
  
  info<-x[[2]]
  x<-x[[1]]
  coefs<-as.data.frame(x$coefficients[1:20])
  coefs$Var<-row.names(coefs)
  
  names(coefs)<-c('Value','Var')
  
  if(type=='RSF'){
    coefs$Model<-gsub('RF_','',coefs$Var[12])
  }else{
    coefs$Model<-'ClosestDist'
    
  }
  coefs$Spp<-info$Spp
  coefs$Year<-info$Year
  coefs$Month<-info$Month
  
  return(coefs)
}