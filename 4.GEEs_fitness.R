#' Here we investigate the relationship between group size and reproductive success by fitting a GEE containing group size 
#' and its quadratic term as predictors of the median number of chicks in the group in a given season. 

library(repmis)
library(MASS)
library(QICpack)
library(geepack)
library(broom)

source_data("https://github.com/DanPapageorgiou/Group_size/blob/master/Computed_seasonal_measurements.Rdata?raw=True")  #Load Computed_seasonal_measurements.RData from GitHub

#' We should include seasons during which the population had chicks. 
#' These are all seasons but A and D (or C & F according to the old system of numbering seasons)

sDCEFGnu$chicks_until_8_months <- 1
sDCEFGnu$chicks_until_8_months[sDCEFGnu$SEASON %in% c("C", "F")] <- 0

sDCEFGnu_ <- sDCEFGnu[sDCEFGnu$chicks_until_8_months == 1,]

m73 <- geeglm(Chicks.as.numeric.Current_chicks. ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 , id=ID, data=sDCEFGnu_, corstr="independence")

m74 <- geeglm(Chicks.as.numeric.Current_chicks. ~ Adults.as.numeric.Adults. , id=ID, data=sDCEFGnu_, corstr="independence")

work.d <- "XXX" # Working directory.

setwd(work.d)

tidy_m <- list()
for (i in 73:74){
  tidy_m[[i]] <- tidy(get(paste("m",i, sep="")))
  gee_model <- get(paste("m",i, sep=""))
  tidy_m[[i]]$QIC <- qic(gee_model)[1,1]
  Y_bar = mean(gee_model$y, na.rm = T)
  tidy_m[[i]]$rsquare_gee <- 1-(sum(gee_model$weights * (gee_model$y - gee_model$fitted.values)^2, na.rm = T)/sum(gee_model$weights*(gee_model$y - Y_bar)^2, na.rm = T))
  write.csv(tidy_m[[i]], paste("tidy_m", i,".csv", sep=""))
}
