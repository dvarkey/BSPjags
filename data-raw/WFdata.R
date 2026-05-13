## code to prepare `WFdata` dataset goes here
witchdata.dat<-read.csv('data-raw/2026 witch 3NO RUN1.csv',head=T)

#create data object
data <- list(N=66, L = (witchdata.dat$Landings),   Isplate = (witchdata.dat$splate), Ifallcam =(witchdata.dat$fallcam), Ispearly=(witchdata.dat$spearly), Icabs=(witchdata.dat$cabs), Icabf=(witchdata.dat$cabf))

# Isplate = ((witchdata.dat$splate))
# Ifallcam = ((witchdata.dat$fallcam))
# Ispearly=((witchdata.dat$spearly))
#
# Blank = (witchdata.dat$Dummy)

inits <- list(
  list(P=c(0.13, 0.10, 0.10, 0.14, 0.22, 0.29, 0.36, 0.52, 0.41, 0.44, 0.37, 0.38, 0.33, 0.29, 0.26, 0.22, 0.31, 0.44, 0.37, 0.37, 0.39, 0.41, 0.36, 0.30, 0.23, 0.33, 0.32, 0.32, 0.34, 0.27, 0.18, 0.10, 0.05, 0.05, 0.03, 0.02, 0.12, 0.10, 0.11, 0.21, 0.17, 0.21, 0.14, 0.18, 0.16, 0.24, 0.24, 0.26, 0.26, 0.14, 0.20, 0.25, 0.25, 0.25, 0.2, 0.3, 0.35, 0.35, 0.36, 0.65, 0.01, 0.33, 0.24, 0.43, 0.36, 0.29), r=0.3, K=100,  pq.splate=4, pq.fallcam=4, pq.spearly=4, pq.cabs=4, pq.cabf=4, sigma=3,  tau.splate=50, tau.fallcam=50, tau.spearly=50, tau.cabs=50, tau.cabf=50),
  list(P=c(0.4, 0.5, 0.6, 0.5, 0.5, 0.5, 0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1, 0.01, 0.01, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.6, 0.6, 0.6, 0.5, 0.5, 0.5, 0.4, 0.1, 0.9, 0.4, 0.4, 0.3, 0.3, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 0.05, 0.1, 0.1, 0.1, 0.15, 0.2, 0.3, 0.2, 0.4, 0.5, 0.3), r=0.4, K=75,  pq.splate=3, pq.fallcam=3, pq.spearly=3, pq.cabs=3, pq.cabf=3, sigma=2,  tau.splate=75, tau.fallcam=75, tau.spearly=75, tau.cabs=75, tau.cabf=75),
  list(P=c(0.13, 0.10, 0.10, 0.14, 0.22, 0.29, 0.36, 0.52, 0.41, 0.44, 0.37, 0.38, 0.33, 0.29, 0.26, 0.22, 0.31, 0.44, 0.37, 0.37, 0.39, 0.41, 0.40, 0.39, 0.38, 0.37, 0.36, 0.32, 0.34, 0.27, 0.18, 0.10, 0.05, 0.05, 0.03, 0.02, 0.12, 0.10, 0.11, 0.21, 0.17, 0.21, 0.14, 0.18, 0.16, 0.24, 0.24, 0.26, 0.26, 0.14, 0.20, 0.25, 0.25, 0.25, 0.2, 0.15, 0.1, 0.09, 0.1, 0.28, 0.42, 0.12, 0.27, 0.19, 0.37, 0.31), r=0.1, K=125, pq.splate=5, pq.fallcam=5, pq.spearly=5, pq.cabs=5, pq.cabf=5, sigma=1, tau.splate=100, tau.fallcam=100, tau.spearly=100, tau.cabs=100, tau.cabf=100))


#list parameters to monitor in the model
parameters <- c('K', 'r', 'pq.splate','pq.fallcam', 'pq.spearly', 'pq.cabs', 'pq.cabf', 'q.splate', 'q.spearly', 'q.fallcam', 'q.cabs', 'q.cabf', 'sigma', 'tau.splate','tau.fallcam', 'tau.spearly', 'tau.cabs', 'tau.cabf', 'Isplatem', 'Ifallcamm',  'Ispearlym', 'Icabsm', 'Icabfm', 'B', 'MSP', 'FMSY', 'BMSY', 'Bratio', 'Fratio', 'F', 'res.Isplate.rep', 'res.Ifallcam.rep', 'res.Ispearly.rep', 'res.Icabs.rep', 'res.Icabf.rep', 'P.res', 'Pin')


WFdata <- list(
  data=data,
  inits=inits,
  parameters=parameters
)

usethis::use_data(WFdata, overwrite = TRUE)
