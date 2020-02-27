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

#run PCA
PCA <- prcomp(means_sub.df, center=T, scale=T, rank=2)

#project a test subset
means.proj <- as.data.frame(predict(PCA,means.df[sample(nrow(means.df), 1000), ]))
plot(proj_sub.df[,1], proj_sub.df[,2])

#now lets average by ecoregion and project these into PCA space

#start with the raster input
fullmean.df<-as.data.frame(meanRast) #this time we'll keep the NAs to preserve cell number
fullmean.df$cell_num<-c(1:(4320*8640))
fullmeanNA.df<-fullmean.df[!is.na(fullmean.df$avetmax), ]
fullmean.df$ECO_ID<-eco_finder(ecoRast_4C)

eco_finder<-function(ecoRast_4C)
{
  for (i in 1:(4320*8640))
}

