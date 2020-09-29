#' The raw GPS data from which we generate ctmm objects are stored on Movebank and can be visualised accessing the study named "Avulturinum_Farine"
#' The URL of the study is: https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study475851705
#' Below we provide the code to calculate the home range (AKDE), daily travelled distance and auto-overlap in space use
#' for each group, using the ctmm package.
#' For each group in each season we used 1 GPS fix every five minutes.
#' Use the same working directory as in script 1.get&prepare_data.R


#' Load dataset for one season ( named "GPSdata"). 
load("5minGPSdata.Rdata")

#' This a list of telemetry objects and each telemetry object contains five minute data 
#' for one individual that represents its group in one season.

library(ctmm)

uere <- 1.297578 #' check ?uere in the ctmm package and Fleming & Calabrese 2019 for a definition and suggestions on how to calculate an uere

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

#' Below we calculate for each group in one season, the average temporal overlap in space use of the first 21 days of the focal season


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












