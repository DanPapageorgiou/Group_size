#' The vulturine guineafowl dataset is stored on movebank and can be visualised accessing the study named "Avulturinum_Farine".
#' Below we calculate for each group in one season, the speed on the go, using high resolution data (one GPS fix every second)

load("all_data.Rdata") # Load dataset ("data"). This is a list of move objects stored in the same working directory as used for scripts "1" and "2".
datas <- data
#' Each object contains raw GPS data from one individual that represents
#' its group in a given season.

library(plyr)
library(ctmm)

datas_ <- as.data.frame(unlist(datas))

ids <- unique(datas_$local_identifier)

datas__ <- list()

for(i in 1: length(ids)){
  a <- datas_[datas_$local_identifier == ids[i], ]
  a$time.diff <- NA
  a$time.diff[2:nrow(a)] <- a$timestamp[2:nrow(a)]-a$timestamp[1:(nrow(a)-1)]
  a$dist <- NA
  a$dist[2:nrow(a)] <- SDMTools::distance(lat1=a$location_lat[2:nrow(a)], lon1=a$location_long[2:nrow(a)], lat2=a$location_lat[1:(nrow(a)-1)], lon2=a$location_long[1:(nrow(a)-1)])$distance
  datas__[[i]] <- a
}  

datas_ <- ldply (datas__, data.frame)
rm(list=setdiff(ls(), c("datas_", "ids")))


datas1sec <- list()
for(i in 1: length(ids)){  
  a <- datas_[datas_$local_identifier == ids[i], ]
  datas1sec[[i]] <-a[which(a$time.diff == 1),]
} 

#' The log(speed) histograms show that all distributions are clearly multimodal. We need all the data while birds are moving. 
#' Thus, we will exclude the first mode when log(dist) < -3.5

datas1sec_ <- list()
meanspgo <- c()

for(i in 1: length(datas1sec)){  
  datas1sec_[[i]] <- datas1sec[[i]][which(log(datas1sec[[i]]$dist) > -3.5),]
  meanspgo[i] <- mean(datas1sec[[i]]$dist) 
}
