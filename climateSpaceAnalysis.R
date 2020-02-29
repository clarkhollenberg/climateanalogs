library(ncdf4)
library(raster)
library(dplyr)
library(scales)

setwd("~/ClimateAnalogs/analysis/Terraclimate") #laptop
setwd("~/Documents/Analogs/ClimateSpace") 

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
#mask areas with insufficient data from analysis in current PCA (to remove rock and ice, etc)
jmask<-raster("/home/clark/Documents/Analogs/InRasters/johnmask.nc")
meanRast_mask<-mask(meanRast, jmask, maskvalue=1)  #to remove insufficient data areas (mostly ice and the amazon)

#subsample cells w/out NA
means.df<-as.data.frame(meanRast_mask, na.rm = TRUE)
colnames(means.df)<-c("tmax", "tmin", "aet", "def")
means_sub.df<-means.df[sample(nrow(means.df), 500000), ]

#run PCA
PCA <- prcomp(means_sub.df, center=T, scale=T, rank=2)

# #project a test subset
# means.proj <- as.data.frame(predict(PCA,means.df[sample(nrow(means.df), 1000), ]))
# plot(proj_sub.df[,1], proj_sub.df[,2])


#now lets average by ecoregion and project these into PCA space
ecorast_now <- raster("/home/clark/Documents/Analogs/InRasters/ecorast_now.tif")
ecorast_2C <- raster("/home/clark/Documents/Analogs/InRasters/ecorast_2C.tif")
ecorast_4C <- raster("/home/clark/Documents/Analogs/InRasters/ecorast_4C.tif")
LUT<-read.csv("LUT.csv")
LUT$X<-NULL





#start with the raster input
extract_ecoids<-function(inputMeanRast, ecorast)
{
  fullmean.df<-as.data.frame(inputMeanRast) #this time we'll keep the NAs to preserve cell number
  fullmean.df$cell_num<-c(1:(4320*8640))
  fullmean.df<-fullmean.df[!is.na(fullmean.df[,1]), ] #remove NA (by checking the first column
  fullmean.df$ECO_ID<-extract(ecorast, fullmean.df$cell_num) #returns current eco_ids
  fullmean.df<-fullmean.df[!is.na(fullmean.df$ECO_ID), ] #Remove the few eco_ids with NA
  fullmean.df$cell_num<-NULL
  return(fullmean.df)
}

input_now.df<- extract_ecoids(meanRast_mask, ecorast_now)
aves_by_eco_now.df<-meanByEco(input_now.df, input_now.df$ECO_ID) 
colnames(aves_by_eco_now.df)<-c("tmax", "tmin", "aet", "def", "ECO_ID")
df<-data.frame('ECO_ID'=0:846)
aves_by_eco_now.df<-merge(df, aves_by_eco_now.df, by="ECO_ID") #to sort by ecoregion
means_now.proj<-as.data.frame(predict(PCA,aves_by_eco_now.df[, 2:5]))
means_now.proj$PC2<-means_now.proj$PC2*-1
means_now.proj$ECO_ID<-aves_by_eco_now.df$ECO_ID
means_now.proj<-merge(means_now.proj, LUT, by="ECO_ID")

#plot
pdf("current_ecorgn_climate_space_masked.pdf", 12, 8)
plot(means_now.proj$PC1, means_now.proj$PC2, col=as.character(means_now.proj$color), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="Current Ecoregions in Climate Space")
dev.off()

#to scatter plot a subset
sub_means.df<-fullmean.df[sample(1:nrow(fullmean.df), 10000), ]
colnames(sub_means.df)<-c("tmax", "tmin", "aet", "def", "ECO_ID")
sub_means.proj<-as.data.frame(predict(PCA,sub_means.df[, 1:4])) #project in PCA space and rotate first component
sub_means.proj$PC1<-sub_means.proj$PC1*-1
sub_means.proj$ECO_ID<-sub_means.df$ECO_ID
sub_means.proj<-merge(sub_means.proj, LUT, by="ECO_ID")
pdf("current_climate_space_subsample.pdf", 12, 8)
plot(sub_means.proj$PC1, sub_means.proj$PC2, col=alpha(as.character(sub_means.proj$color), 0.8), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="10000 cells in current Climate Space")
dev.off()

#by biome
pdf("current_climate_space_subsample_biome.pdf", 12, 8)
plot(sub_means.proj$PC1, sub_means.proj$PC2, col=as.character(sub_means.proj$biome_color), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="10000 cells in current Climate Space")
legend("topleft", legend=unique(LUT$BIOME_NAME), cex=0.75, fill=as.character(unique(LUT$biome_color)))
dev.off()
#for 2C condition



meanByEco<-function(data, indexCol)
{
  out<-data.frame()
  for (i in unique(indexCol))
  {
    df<-subset(data, ECO_ID==i)
    vec<-apply(df[, 1:4], 2, mean) %>% c(., i)
    out<-rbind(out, vec)
    print(i)
  }
  return(out)
}
