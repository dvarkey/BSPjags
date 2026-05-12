
#' Calculate biomass in next year using estimated carrying capacity and growth rate
#'
#' @param Bt scalar. Current level of biomass from which to calculate next year biomass
#' @param Ct scalar. Current level of catches from which to calculate next year biomass
#' @param r scalar. Growth rate
#' @param K scalar. Carrying capacity
#' @references copied from Kumar and Varkey RMSE package
#' 
#' @return scalar. Biomass in next year
#' @export
#'
#' @examples
#'
calcBt <- function(Bt, Ct, r, K){
  return(Bt + r*Bt*(1-Bt/K) - Ct)
}



#' Gets output from Jags object
#'
#' @param modelRunOutput JAGS model output object
#'
#' @returns summarized model output
#' @export
#'
#' @examples
getOutputs <- function(modelRunOutput) {
  
  ret <- list()
  
  # Last row of B matrix/array
  ret$Bterminal <- median(modelRunOutput$B[nrow(modelRunOutput$B), ,] )
  ret$Fterminal <- median(modelRunOutput$F[nrow(modelRunOutput$B), ,] )
  
  ret$BMSY     <- median(modelRunOutput$BMSY)
  ret$Blim     <- ret$BMSY * 0.3
  ret$Btrigger <- ret$BMSY * 0.75
  ret$FMSY     <- median(modelRunOutput$FMSY)
  ret$Ftarget  <- ret$FMSY * 0.85
  
  return(ret)
}
