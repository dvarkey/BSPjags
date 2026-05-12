
#' Title This function calculates F for a linear HCR
#'
#' @param Bt : stock biomass at time t
#' @param Blim : Biomass limit reference point (upper boundary for critical zone)
#' @param Btrigger : Biomass trigger reference point (upper boundary for cautious zone)
#' @param Fmin : defaut Fmin=0.001.  A very small value of F set at the critical zone. Setting Fmin=0, sometime causes trouble
#' @param ...
#' @param Ftarget : Target Fishing mortality
#' @references copied from Kumar and Varkey RMSE package
#' @return
#' @export
#'
#' @examples
#'
Flinear<-function(Bt,Blim,Btrigger,Ftarget,Fmin=0.001,...){
  cautious<-1 # internal function flag to indicate if the stock is within the cautious zone

  if(Bt<=Blim){  #stock in the critical zone, set F=0
    F_t<-Fmin
    cautious<-0
    return(F_t)}
  if(Bt>=Btrigger){  #stock in the healthy zone, set F=Ftarget
    F_t<-Ftarget
    cautious<-0
    return(F_t)}

  if(cautious==1){#Stock in the cautious zone, Linear HCR
    slope<-Ftarget/(Btrigger-Blim)
    intercept<-(-1.0)*slope*Blim
    F_t<-slope*Bt+intercept
    return(F_t)}
}



#' Title This function calculates F for a sigmoid HCR
#'
#' @inherit Flinear
#' @param x50 : Distance from Blim where F=0.5*Ftarget expressed as fraction of cautious zone biomass range. For Upper leaf: 0>x50>0.5 and for Lower leaf: 0.5>x50>1
#' @param ... :useful if uses as a nested function (inside a function)
#' @param k : shape parameter for the sigmoid HCR
#'
#' @references copied from Kumar and Varkey RMSE package
#' @return
#' @export
#'
#' @examples
#'
Fsigmoid<-function(Bt,Blim,Btrigger,x50,Ftarget,k=1,Fmin=0.001,...){


  cautious<-1 # internal function flag to indicate if the stock is within the cautious zone

  if(Bt<=Blim){  #stock in the critical zone, set F=0
    F_t<-Fmin
    cautious<-0
    return(F_t)}
  if(Bt>=Btrigger){  #stock in the healthy zone, set F=Ftarget
    F_t<-Ftarget
    cautious<-0
    return(F_t)}

  if(cautious==1){#stock in the cautious zone, calculate Sigmoid HCR
    # calculation of Bx
    B_range<-Btrigger-Blim
    B_50 <- x50*B_range+Blim

    aBx<-(B_50-Blim)^k
    bBx<-(Btrigger-Blim)^k
    Bx<-Blim+((aBx*bBx)/(bBx-2*aBx))^(1/k)
    # calculation of A
    cA<-(Bx-Blim)^k
    A<-(Ftarget*(cA+bBx))/bBx
    # calculation of Ft
    dBt<-(Bt-Blim)^k
    F_t<-(A*dBt)/(cA+dBt)
    return(F_t)
  }
}



#' Function caller.
#'
#' Call any R function by the function name in quote followed by that function args, separated by commas
#'
#' @param fnName Name of the function to be called in quote
#' @param ... arguments require for the function to be called
#' @references copied from Kumar and Varkey RMSE package
#' 
#' @return Output of the actual function
#' @export
#'
#' @examples
#' fnName="mean"
#' x=c(2,5,6,NA)
#' match.fun(fnName)(x,na.rm=TRUE)
#'
fnCall <- function(fnName,...){
 return(match.fun(fnName)(...))

}



#' Plot PA leaf
#'
#' @inherit Flinear
#' @param Blim B
#' @param Btrigger
#' @param Ftarget
#' @param x50UL Distance from Blim where F=0.5*Ftarget expressed as fraction of cautious zone biomass range. For Upper leaf: 0>x50>0.5
#' @param x50ML Distance from Blim where F=0.5*Ftarget expressed as fraction of cautious zone biomass range. For mid leaf: x50=0.5
#' @param x50LL Distance from Blim where F=0.5*Ftarget expressed as fraction of cautious zone biomass range. Lower leaf: 0.5>x50>1
#' @param Fmin
#' @import ggplot2
#' 
#' @references copied from Kumar and Varkey RMSE package
#' @return
#' @export
#'
#' @examples
plotLeaf <- function(Blim = 12240, Btrigger = 22949, Ftarget = 0.269, x50UL = 0.25, x50ML = 0.5, x50LL = 0.75,Fmin=0.001) {
  Bt <- seq(0, Btrigger * 1.5, 100)

  Fbar1 <- purrr::map_dbl(Bt, ~ RMSE::Fsigmoid(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50UL, Ftarget = Ftarget, k = 1, Fmin = Fmin))
  Fbar2 <- purrr::map_dbl(Bt, ~ RMSE::Flinear(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50ML, Ftarget = Ftarget, k = 1, Fmin = Fmin))
  Fbar3 <- purrr::map_dbl(Bt, ~ RMSE::Fsigmoid(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50LL, Ftarget = Ftarget, k = 1, Fmin = Fmin))

  Fbar <- dplyr::bind_rows(
    dplyr::tibble(Fbar = Fbar1) %>% dplyr::mutate(x50 = x50UL),
    dplyr::tibble(Fbar = Fbar2) %>% dplyr::mutate(x50 = x50ML),
    dplyr::tibble(Fbar = Fbar3) %>% dplyr::mutate(x50 = x50LL)
  )

  df1 <- data.frame(Fbar, Bt)

  plot <- df1 %>%
    ggplot(aes(x = Bt, y = Fbar, group = x50)) +
    geom_rect(aes(xmin = 0, xmax = Blim, ymin = 0, ymax = Ftarget / 0.7, fill = "Critical Zone"), alpha = 0.5) +
    geom_rect(aes(xmin = Blim, xmax = Btrigger, ymin = 0, ymax = Ftarget / 0.7, fill = "Cautious Zone"), alpha = 0.5) +
    geom_rect(aes(xmin = Btrigger, xmax = Btrigger * 1.5, ymin = 0, ymax = Ftarget / 0.7, fill = "Healthy Zone"), alpha = 0.5) +
    geom_line() +
    scale_fill_manual(values = c("Critical Zone" = "lightpink2", "Cautious Zone" = "khaki1", "Healthy Zone" = "lightgreen")) +
    geom_vline(xintercept = c(Blim, Btrigger), linetype = "dotted") +
    geom_hline(yintercept = c(Ftarget, Ftarget / 0.85), linetype = "dotted") +
    labs(x = "SSB", y = "Fishing mortality") +
    annotate("text", x = c(Blim, Btrigger), y = 0, label = c("Blim", "Btrigger"), vjust = 1.4) +
    annotate("text", x = 0, y = c(Ftarget, Ftarget / 0.85), label = c("Ftarget", "Flim"), angle = 90, vjust = -1) +
    annotate("text", x = c(Blim/2, Blim+(Btrigger-Blim)/2, Btrigger+(Btrigger * 0.25)), y = Ftarget / 0.75, label = c("Critical Zone", "Cautious Zone", "Healthy Zone"), hjust = 'center', size = 4) +
    theme_bw() +
    theme(legend.position = "none")

  print(plot)
}


#' Title
#'
#' @param Blim
#' @param Btrigger
#' @param Ftarget
#' @param x50UL
#' @param x50ML
#' @param x50LL
#' @param Fmin
#' @param stockval Current stock level to be indicated
#' @param stockF
#' @param xtxt
#' @references copied from Kumar and Varkey RMSE package
#' @return
#' @export
#'
#' @examples
plotLeafStock <- function(Blim = 12240, Btrigger = 22949, Ftarget = 0.269, x50UL = 0.25, x50ML = 0.5, x50LL = 0.75,Fmin=0.001,stockval=NULL,stockF=NULL,xtxt="SSB") {
  Bt <- seq(0, Btrigger * 1.5, length.out =1000)

  Fbar1 <- purrr::map_dbl(Bt, ~ RMSE::Fsigmoid(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50UL, Ftarget = Ftarget, k = 1, Fmin = Fmin))
  Fbar2 <- purrr::map_dbl(Bt, ~ RMSE::Flinear(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50ML, Ftarget = Ftarget, k = 1, Fmin = Fmin))
  Fbar3 <- purrr::map_dbl(Bt, ~ RMSE::Fsigmoid(.x, Blim = Blim, Btrigger = Btrigger, x50 = x50LL, Ftarget = Ftarget, k = 1, Fmin = Fmin))

  Fbar <- dplyr::bind_rows(
    dplyr::tibble(Fbar = Fbar1) %>% dplyr::mutate(x50 = x50UL),
    dplyr::tibble(Fbar = Fbar2) %>% dplyr::mutate(x50 = x50ML),
    dplyr::tibble(Fbar = Fbar3) %>% dplyr::mutate(x50 = x50LL)
  )

  df1 <- data.frame(Fbar, Bt)
  plot <- df1 %>%
    ggplot(aes(x = Bt, y = Fbar, group = x50)) +
    geom_rect(aes(xmin = 0, xmax = Blim, ymin = 0, ymax = Ftarget / 0.7, fill = "Critical Zone"), alpha = 0.5) +
    geom_rect(aes(xmin = Blim, xmax = Btrigger, ymin = 0, ymax = Ftarget / 0.7, fill = "Cautious Zone"), alpha = 0.5) +
    geom_rect(aes(xmin = Btrigger, xmax = Btrigger * 1.5, ymin = 0, ymax = Ftarget / 0.7, fill = "Healthy Zone"), alpha = 0.5) +
    geom_line() +
    scale_fill_manual(values = c("Critical Zone" = "lightpink2", "Cautious Zone" = "khaki1", "Healthy Zone" = "lightgreen")) +
    geom_vline(xintercept = stockval, linetype = "dashed",colour="blue")+
    geom_hline(yintercept = stockF, linetype = "dashed",colour="blue") +
    geom_vline(xintercept = c(Blim, Btrigger), linetype = "dotted") +
    geom_hline(yintercept = c(Ftarget, Ftarget / 0.85), linetype = "dotted") +
    geom_point(aes(x=stockval,y=stockF),col='darkorange',fill='darkorange',size=2)+
    labs(x = xtxt, y = "Fishing mortality") +
    annotate("text", x = c(Blim, Btrigger), y = 0, label = c("Blim", "Btrigger"), vjust = 1.4) +
    annotate("text", x = 0, y = c(Ftarget, Ftarget / 0.85), label = c("Ftarget", "Flim"), angle = 90, vjust = -1) +
    annotate("text", x = c(Blim/2, Blim+(Btrigger-Blim)/2, Btrigger+(Btrigger * 0.25)), y = Ftarget / 0.75, label = c("Critical Zone", "Cautious Zone", "Healthy Zone"), hjust = 'center', size = 4) +
    annotate("text", x = 0, y = stockF, label ="F", angle = 90, vjust = -1,col='blue') +
    annotate("text", x = stockval, y = 0, label ="B", vjust = 1.4,col='blue') +
    theme_bw() +
    theme(legend.position = "none")

  print(plot)
}


