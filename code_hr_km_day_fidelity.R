#'The data to generate ctmm objects are stored on movebank and can be visualised accessing the study named "Avulturinum_Farine".
#'Below we provide the code to calculate the home range (AKDE), daily travelled distance and  daily auto-overlap
#'for each group using the ctmm package
#'For each group in each season we used 1 GPS fix every five minutes. 

#' load uere (check ctmm package and Fleming & Calabrese 2019)
#' load dataset for one season (GPSdata). This a list of telemetry objects and
#' each telemetry obsject contains five minute data for one individual that represents its group in one season.

library(ctmm)

AREA_SQM <- c()
speed_km_day <- c()

for (i in 1:length(GPSdata)){
  a <- GPSdata[[i]]
  GUESS <- ctmm.guess(a,interactive=FALSE)
  FITS <- ctmm.fit(a,GUESS)
  sum <- summary(FITS, units=F)
  

  AREA_SQM[i]  <- sum[[3]][1,2]
  
  speed_km_day[i] <- speed(FITS, a)[1,2]
}

#' below we calculate for each group in one season, the daily overlap of the first 21 days of the focal season


library(dplyr)


autoverlap <-  array(NA, dim = c(21, 21, length(GPSdata)))


for (i in 1:length(GPSdata)){
  a <- unique(as.Date(GPSdata[[i]]$timestamp))
  for (j in 1:20){
    for (k in (j+1):21){
      if(k-j <=3){
        c <- GPSdata[[i]][which(as.Date(GPSdata[[i]]$timestamp) ==  a[j]),]
        d <- GPSdata[[i]][which(as.Date(GPSdata[[i]]$timestamp) ==  a[k]),]
        
        
        GUESSc <- ctmm.guess(c,interactive=FALSE)
        FITSVFc <- ctmm.fit(c,GUESSc)
        GUESSd <- ctmm.guess(d,interactive=FALSE)
        FITSVFd <- ctmm.fit(d,GUESSd)
        
        b <- overlap(list(FITSVFc, FITSVFd))
        autoverlap[j,k,i] <-   b[1,2,2]
        print(paste("group", i, "day", j, "to day", k))
      }
    }
  }
}

fidelity <- c()
for (i in 1 :length(GPSdata)){
  a <-autoverlap[,,i]
  fidelity[i] <- mean(a, na.rm = T)
}

#' below we calculate for each group in one season, the speed on the go using high resolution data (one second)










