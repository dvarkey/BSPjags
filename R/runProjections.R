

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
Fp=Bp
Y=length(output$B[,1,1])
Bp[1,,]=calcBt(output$B[Y,,],Ct=Yield[1],r=output$r[1,,],K=output$K[1,,])
Fp[1,,]=Yield[1]/Bp[1,,]
  for(i in 2:nY){
    Bp[i,,]=calcBt(Bp[i-1,,],Ct=Yield[i],r=output$r[1,,],K=output$K[1,,])
    Fp[i,,]=Yield[i]/Bp[i,,]
  } 
Bproj <- t(apply(Bp, 1, function(x) {
  c(median = median(x),
    l80 = quantile(x, 0.10),
    u80 = quantile(x, 0.90))
}))
Fproj <- t(apply(Fp, 1, function(x) {
  c(median = median(x),
    l80 = quantile(x, 0.10),
    u80 = quantile(x, 0.90))
}))
return(list(Bproj=Bproj,Fproj=Fproj))

}


