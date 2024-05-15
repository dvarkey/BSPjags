## code to prepare `WFdata` dataset goes here
witchdata.dat<-read.csv('data-raw/witch splate fall spearly 2022.csv',head=T)

#create data object
data <- list(N=62, L = (witchdata.dat$Landings),   Isplate = (witchdata.dat$splate),
             Ifallcam =(witchdata.dat$fallcam), Ispearly=(witchdata.dat$spearly))

# Isplate = ((witchdata.dat$splate))
# Ifallcam = ((witchdata.dat$fallcam))
# Ispearly=((witchdata.dat$spearly))
#
# Blank = (witchdata.dat$Dummy)

inits <- list(
  list(P=c(0.13, 0.10, 0.10, 0.14, 0.22, 0.29, 0.36, 0.52, 0.41, 0.44, 0.37, 0.38, 0.33, 0.29, 0.26, 0.22, 0.31,
           0.44, 0.37, 0.37, 0.39, 0.41, 0.36, 0.30, 0.23, 0.33, 0.32, 0.32, 0.34, 0.27, 0.18, 0.10, 0.05, 0.05, 0.03, 0.02,
           0.12, 0.10, 0.11, 0.21, 0.17, 0.21, 0.14, 0.18, 0.16, 0.24, 0.24, 0.26, 0.26, 0.14, 0.20, 0.25, 0.25, 0.25, 0.2,
           0.3, 0.35, 0.35, 0.36, 0.65, 0.01, 0.33), r=0.3, K=100,  pq.splate=4, pq.fallcam=4, pq.spearly=4,
       sigma=3,  tau.splate=50, tau.fallcam=50, tau.spearly=50),
  list(P=c(0.4, 0.5, 0.6, 0.5, 0.5, 0.5, 0.4, 0.4, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1, 0.01, 0.01, 0.1,
           0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.6,
           0.6, 0.6, 0.5, 0.5, 0.5, 0.4, 0.1, 0.9, 0.4, 0.4, 0.3, 0.3, 0.3, 0.3, 0.2, 0.2, 0.1, 0.1, 0.1, 0.05, 0.1, 0.1, 0.1, 0.15, 0.2, 0.34),
       r=0.4, K=75,  pq.splate=3, pq.fallcam=3, pq.spearly=3,
       sigma=2,  tau.splate=75, tau.fallcam=75, tau.spearly=75),
  list(P=c(0.13, 0.10, 0.10, 0.14, 0.22, 0.29, 0.36, 0.52, 0.41, 0.44, 0.37, 0.38, 0.33, 0.29, 0.26, 0.22, 0.31,
           0.44, 0.37, 0.37, 0.39, 0.41, 0.40, 0.39, 0.38, 0.37, 0.36, 0.32, 0.34, 0.27, 0.18, 0.10, 0.05, 0.05, 0.03, 0.02,
           0.12, 0.10, 0.11, 0.21, 0.17, 0.21, 0.14, 0.18, 0.16, 0.24, 0.24, 0.26, 0.26, 0.14, 0.20, 0.25, 0.25, 0.25, 0.2,
           0.15, 0.1, 0.09, 0.1, 0.28, 0.42, 0.12), r=0.1, K=125, pq.splate=5, pq.fallcam=5, pq.spearly=5,
       sigma=1, tau.splate=100, tau.fallcam=100, tau.spearly=100))



#list parameters to monitor in the model
parameters <- c('K', 'r', 'pq.splate','pq.fallcam', 'pq.spearly', 'q.splate', 'q.spearly', 'q.fallcam',  'sigma',
                'tau.splate','tau.fallcam',  'tau.spearly',
                'Isplatem', 'Ifallcamm',  'Ispearlym',  'B', 'MSP', 'FMSY', 'BMSY', 'Bratio', 'Fratio', 'F',
                'res.Isplate.rep', 'res.Ifallcam.rep', 'res.Ispearly.rep',  'P.res', 'Pin')


WFdata <- list(
  data=data,
  inits=inits,
  parameters=parameters
)

usethis::use_data(WFdata, overwrite = TRUE)
