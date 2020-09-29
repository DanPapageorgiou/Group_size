library(repmis)
library(MASS)
library(QICpack)
library(geepack)
library(broom)

work.d <- "XXX" #Set the working directory where the results of the GEEs will be saved
setwd(work.d)

source_data("https://github.com/DanPapageorgiou/Group_size/blob/master/Computed_seasonal_measurements.Rdata?raw=True")  #Load Computed_seasonal_measurements.RData from GitHub


############Simplest GEEs, no interactions, no chicks square###########################

#Home range

m1 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m2 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m3 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

m4 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 , id=ID, data=sDCEFGnu, corstr="independence")

#Average temporal overlap in space use

m5 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m6 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 , id=ID, data=sDCEFGnu, corstr="independence")

m7 <- geeglm(fidelity ~ Adults.as.numeric.Adults. , id=ID, data=sDCEFGnu, corstr="independence")

m8 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

#Average daily distance travelled

m9 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2  + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m10 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 , id=ID, data=sDCEFGnu, corstr="independence")

m11 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. , id=ID, data=sDCEFGnu, corstr="independence")

m12 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu,corstr="independence")

#Speed while Travelling

m13 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m14 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 , id=ID, data=sDCEFGnu, corstr="independence")

m15 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m16 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. , id=ID, data=sDCEFGnu, corstr="independence")


#####################GEEs with interactions######################

#Home range

m17 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m18 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m19 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m20 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m21 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m22 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m23 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

m24 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults.  + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

#Average temporal overlap in space use

m26 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m27 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m28 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m29 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m30 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m31 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m32 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

m33 <- geeglm(fidelity ~ Adults.as.numeric.Adults.  + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

#Average daily distance travelled

m35 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m36 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m37 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m38 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m39 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m40 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m41 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

m42 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults.  + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

#Speed while Travelling

m44 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m45 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m46 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m47 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m48 <- geeglm(meanspgo~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m49 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks.+ Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m50 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

m51 <- geeglm(meanspgo ~ Adults.as.numeric.Adults.  + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks.:Adults.as.numeric.Adults., id=ID, data=sDCEFGnu, corstr="independence")

#####################GEEs with chicks square######################
sDCEFGnu$Chicks.as.numeric.Current_chicks_2 <- sDCEFGnu$Chicks.as.numeric.Current_chicks.^2

#Home range

m53 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m54 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m55 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m56 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m57 <- geeglm(AREA_SQM ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

#Average temporal overlap in space use
m58 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m59 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m60 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m61 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m62 <- geeglm(fidelity ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

#Average daily distance travelled
m63 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m64 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m65 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m66 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m67 <- geeglm(speed_km_day ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

#Speed while Travelling
m68 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks. + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m69 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults. :Chicks.as.numeric.Current_chicks., id=ID, data=sDCEFGnu, corstr="independence")

m70 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m71 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks_2 + Adults.as.numeric.Adults_2 :Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")

m72 <- geeglm(meanspgo ~ Adults.as.numeric.Adults. + Adults.as.numeric.Adults_2 + Chicks.as.numeric.Current_chicks. + Chicks.as.numeric.Current_chicks_2, id=ID, data=sDCEFGnu, corstr="independence")


tidy_m <- list()

seq <- c(1:24, 26:33, 35:42, 44:51, 53:72) # Models 25,34,43,52 were not considered because they would include only the interactions between adults and chicks and no other variables, which would not be biological relevant

tidy_m <- list()
for (i in seq){
  tidy_m[[i]] <- tidy(get(paste("m",i, sep="")))
  gee_model <- get(paste("m",i, sep=""))
  tidy_m[[i]]$QIC <- qic(gee_model)[1,1]
  Y_bar = mean(gee_model$y, na.rm = T)
  tidy_m[[i]]$rsquare_gee <- 1-(sum(gee_model$weights * (gee_model$y - gee_model$fitted.values)^2, na.rm = T)/sum(gee_model$weights*(gee_model$y - Y_bar)^2, na.rm = T))
  write.csv(tidy_m[[i]], paste("tidy_m", i,".csv", sep=""))
}
