

#' Title
#'
#' @param nY 
#' @param output 
#' @param Yield 
#'
#' @return
#' @export
#'
#' @examples
runProjections <- function(nY=3,output,Yield){
Bp=array(data=0,dim=c(nY,length(output$r[1,,1]),length(output$r[1,1,])))
Y=length(output$B[,1,1])
Bp[1,,]=RMSE::calcBt(output$B[Y,,],Ct=Yield[1],r=output$r[1,,],K=output$K[1,,])
  for(i in 2:nY){
    Bp[i,,]=RMSE::calcBt(Bp[i-1,,],Ct=Yield[i],r=output$r[1,,],K=output$K[1,,])
  } 
Bmsy=median(output$BMSY)
Fmsy=median(output$FMSY)
return(list(Bp=Bp,Bmsy=Bmsy,Fmsy=Fmsy))

}


