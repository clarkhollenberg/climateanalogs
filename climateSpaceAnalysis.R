library(ncdf4)
library(raster)
library(dplyr)


setwd("~/ClimateAnalogs/analysis/Terraclimate")
#load in Terraclimate data -skip
#####
tmax<-brick('terra1961990_txx.nc')
tmin<-brick('terra1961990_tnn.nc')
aet<-brick('terra1961990_aet.nc')
def<-brick('terra1961990_def.nc')
#####

#create 30yr climate averages -skip and load tifs!
#####
tmax.av<-calc(tmax, fun=mean)
writeRaster(tmax.av, "avetmax.tif", format="GTiff", overwrite=T)
tmin.av<-calc(tmin, fun=mean)
writeRaster(tmin.av, "avetmin.tif", format="GTiff", overwrite=T)
aet.av<-calc(aet, fun=mean)
writeRaster(aet.av, "aveaet.tif", format="GTiff", overwrite=T)
def.av<-calc(def, fun=mean)
writeRaster(def.av, "avedef.tif", format="GTiff", overwrite=T)
#####

tmax.av<-raster("avetmax.tif")
tmin.av<-raster("avetmin.tif")
aet.av<-raster("aveaet.tif")
def.av<-raster("avedef.tif")
#stack and convert to df
meanRast<-brick(tmax.av, tmin.av, aet.av, def.av)


#subsample cells w/out NA
means.df<-as.data.frame(meanRast, na.rm = TRUE)
means_sub.df<-means.df[sample(nrow(means.df), 500000), ]


#convert observations to standardized anomalies
sd<-apply(means_sub.df,2, sd)
mean<-apply(means_sub.df,2, mean)
meansprime.df<-sweep(means_sub.df, MARGIN=2, mean,`-`) %>%
                sweep(., MARGIN=2, sd,`/`)
# write.csv(meansprime.df, "climMeans_prime_df.csv")


#run PCA
PCA <- prcomp(means_sub.df, center=T, scale=T)
means.proj <- as.data.frame(predict(PCA,meansprime.df))

proj_sub.df<-means.proj[sample(nrow(means.proj), 2000), ]

plot(proj_sub.df[,1], proj_sub.df[,2])
