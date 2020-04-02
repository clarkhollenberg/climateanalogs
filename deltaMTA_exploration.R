#exploration of latitudinal and other explanations of extreme delta MTA values
PA_perc<-read.csv("Tables/PA_perc_comparison_master.csv")
centr_now<-read.csv("Tables/Current_ecoregion_centroids.csv")
centr_now['X']<-NULL
colnames(centr_now)=c("ECO_ID", "lon", "lat")
mta_eco_delta<-merge(mta_eco_delta, centr_now, by="ECO_ID")
mta_eco_delta<-merge(mta_eco_delta, PA_perc[c("ECO_ID", "tot_area_current")], by="ECO_ID")

#regress latitude as predictor of eco size
lmSizeLat<-lm(tot_area_current ~ abs(lat), data=mta_eco_delta) #p=5.5e-05, R^2=0.021
#regress size independent of latitude as predictor of delta MTA
lmresidSize2C<-lm(abs(mta_eco_delta$deltaMTA_2C) ~ lmSizeLat[[2]]) #p=5.5e-05, R^2=0.021
lmresidSize4C<-lm(abs(mta_eco_delta$deltaMTA_4C) ~ lmSizeLat[[2]]) #p=0.0012, R^2=0.014

#regress eco size as predictor of latitude
lmLatSize<-lm(abs(lat) ~ tot_area_current, data=mta_eco_delta) #p=5.5e-05, R^2=0.02
#regress latitude independent of size as predictor of delta MTA
lmresidlat2C<-lm(abs(mta_eco_delta$deltaMTA_2C) ~ lmLatSize[[2]]) #p=0.125, R^2=0.003
lmresidlat4C<-lm(abs(mta_eco_delta$deltaMTA_4C) ~ lmLatSize[[2]]) #p=0.97, R^2=1.6e-6

lmarea<-lm(abs(deltaMTA_2C) ~ abs(lat), data=mta_eco_delta)
plot(abs(mta_eco_delta$lat), abs(mta_eco_delta$deltaMTA_2C))

plot(abs(mta_eco_delta$lat), mta_eco_delta$tot_area_current, ylim=c(0,1e+06))
lmarea<-lm(tot_area_current ~ abs(lat), data=mta_eco_delta)

