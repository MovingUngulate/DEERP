#' @title Trajectory Info
#
#' @description Add adehabitatLT trajectory vars to data
#' @param df dataframe of relocation data
#' @param timename column name of posix time
#' @param xname column name of x coordinate
#' @param yname column name of y coordinate
#' @param idname column name for individual ID
#' @return Returns data with movement metrics appended
#' @keywords trajectory, traj
#' @export
#' @examples
#' \donttest{traj_fun(df, 'TelemDate','Easting','Northing','AID')}


traj_fun<-function(df, timename, xname, yname, idname){
  
  df$chk<-paste0(df[,idname],'_',df[,timename])
  df<-df[!duplicated(df$chk),]
  
  ti<-Part::trajfun(df,timename=timename,xname=xname,yname=yname,idname=idname)


  outtra <- data.frame()
  for (i in 1:length(unique(ti$id))) {
    itraj <- ti[which(ti$id == unique(ti$id)[i]), ]
    ffu <- df[which(df[,idname] == unique(ti$id)[i]), ]
    itraj <- cbind(ffu, itraj)
    
    itraj <- itraj[, 1:22]
    
    outtra <- rbind(outtra, itraj)
  }
  return(outtra)
}