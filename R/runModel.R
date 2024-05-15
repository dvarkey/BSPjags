
#' Runs Jags model
#'
#' @param modeltxt
#' @param data
#' @param inits
#' @param parameters
#' @param n.chains
#' @param n.adapt
#' @param n.burnin
#' @param n.iter
#'
#' @return
#' @export
#'
#' @examples
runModel <- function(modeltxt, data,inits, parameters, n.chains, n.adapt, n.burnin, n.iter){
  m1 <- rjags::jags.model(modeltxt,dat=data, inits=inits, n.chains=n.chains, n.adapt=n.adapt)
  update(m1, n.iter=n.burnin)
  output <- rjags::jags.samples(m1, variable.names=parameters, n.iter=n.iter, thin=n.thin)
  return(output)
}







