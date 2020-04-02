library(ncdf4)
library(raster)
library(dplyr)
library(scales)

setwd("~/ClimateAnalogs/analysis/Terraclimate") #laptop
setwd("ClimateSpace") 

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
jmask<-raster("Rasters/johnmask.nc")
meanRast_mask<-mask(meanRast, jmask, maskvalue=1)  #to remove insufficient data areas (mostly ice and the amazon)
means.df<-as.data.frame(meanRast_mask, na.rm = TRUE)
colnames(means.df)<-c("tmax", "tmin", "aet", "def")
write.csv(means.df, "climateMeans_wout_NA.csv", row.names=F)

#Load in averages and subsample cells w/out NA
means.df<-read.csv("ClimateSpace/Tables/climateMeans_wout_NA.csv")
means_sub.df<-means.df[sample(nrow(means.df), 500000), ]

#run PCA
PCA <- prcomp(means_sub.df, center=T, scale=T, rank=2)

# #project a test subset
# means.proj <- as.data.frame(predict(PCA,means.df[sample(nrow(means.df), 1000), ]))
# plot(proj_sub.df[,1], proj_sub.df[,2])


#project into PC space and assign ECOIDS
###############################
ecorast_now <- raster("Rasters/ecorast_now.tif")
#note using rasters w/ no analog & insufficient data cells assigned to 847 and 848 respectively
ecorast_2C_mapped <- raster("/home/clark/Documents/Analogs/InRasters/ecorast_2C_mapped.tif") 
ecorast_4C_mapped <- raster("/home/clark/Documents/Analogs/InRasters/ecorast_4C_mapped.tif")
LUT<-read.csv("LUT.csv")
LUT_plus<-read.csv("LUT.csv")

#functions
extract_ecoids<-function(inputMeanRast, ecorast)
{
  PA_bin[is.na(PA_bin)]=0
  df<-as.data.frame(inputMeanRast) #this time we'll keep the NAs to preserve cell number (not using means.df)
  df$cell_num<-c(1:(4320*8640))
  df<-df[!is.na(df[,1]), ] #remove NA (by checking the first column
  df$ECO_ID<-extract(ecorast, df$cell_num) #returns current eco_ids
  df$PA<-extract(PA_bin, df$cell_num)
  df<-df[!is.na(df$ECO_ID), ] #Remove the few eco_ids with NA
  colnames(df)<-c("tmax", "tmin", "aet", "def", "cell_num", "ECO_ID", "PA")
  return(df)
}

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
  colnames(out)<-c("tmax", "tmin", "aet", "def", "ECO_ID")
  df<-data.frame("ECO_ID"=0:847)
  out<-merge(df, out, by="ECO_ID")
  return(out)
}

projectCSpace<-function(inputDF)
{
  input_mask<-inputDF
  input_mask[c('ECO_ID', 'PA')]<-NULL
  proj.df<-as.data.frame(predict(PCA,input_mask))  #don't pass the ECO_ID column to PCA
  proj.df$PC2<-proj.df$PC2*-1  #flip coordinate sign for more intuitive plotting
  proj.df$ECO_ID<-inputDF$ECO_ID  #add the ecoids back in for plotting purposes
  proj.df$PA<-inputDF$PA
  proj.df<-merge(proj.df, LUT_plus, by="ECO_ID") #merge with LUT to get colors etc
  return(proj.df)
}


#assign ecoids from climate scenarios to global climate average dataframe
ecomatched_now.df<- extract_ecoids(meanRast_mask, ecorast_now)
ecomatched_now.df$cell_num<-NULL
write.csv(ecomatched_now.df, "ClimateSpace/Tables/assignedEcoids_now.csv", row.names = F)
ecomatched_now.df<-read.csv("ClimateSpace/Tables/assignedEcoids_now.csv")

##Need to use a different meanRast input for the 2C and 4C climate scenarios
ecomatched_2C.df<- extract_ecoids(meanRast_mask, ecorast_2C_mapped)
ecomatched_4C.df<- extract_ecoids(meanRast_mask, ecorast_4C_mapped)

#project all cells into climate space and use this as framework for further analysis
now_all.proj<-projectCSpace(ecomatched_now.df)
write.csv(now_all.proj, "ClimateSpace/Tables/now.proj.csv", row.names = F)


###########################


#average climate variables by ecoregion
aves_by_eco_now.df<-meanByEco(ecomatched_now.df, ecomatched_now.df$ECO_ID)
write.csv(aves_by_eco_now.df, "Tables/climateMeansbyEco_nowDF.csv", row.names = F)

#need to use different input
aves_by_eco_2C.df<-meanByEco(ecomatched_2C.df, ecomatched_2C.df$ECO_ID) 
aves_by_eco_4C.df<-meanByEco(ecomatched_4C.df, ecomatched_4C.df$ECO_ID) 


pa_all.proj<-subset(now_all.proj, PA==1)


#plotting and visualization
##############################

now_sub.proj<- now_all.proj[sample(1:nrow(now_all.proj), 100000), ]
pa_sub.proj<-pa_all.proj[sample(1:nrow(pa_all.proj), 20000), ]
pdf("Figures/current_PA_comparison.pdf", 18, 8)
par(mfrow=c(1,2))
plot(now_sub.proj$PC1, now_sub.proj$PC2, col=alpha(as.character(now_sub.proj$color), 0.7), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="Subset Global Climate Space")
plot(pa_sub.proj$PC1, pa_sub.proj$PC2, col=alpha(as.character(pa_sub.proj$color), 0.7), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="Subset PA Climate Space")
dev.off()

pdf("ClimateSpace/Figures/current_climate_space_subsample.pdf", 12, 8)
plot(now_sub.proj$PC1, now_sub.proj$PC2, col=alpha(as.character(now_sub.proj$color), 0.8), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="10000 cells in current Climate Space")
dev.off()

#by biome
pdf("Figures/current_climate_space_subsample_biome.pdf", 12, 8)
plot(now_sub.proj$PC1, now_sub.proj$PC2, col=alpha(as.character(now_sub.proj$biome_color), 0.8), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="10000 cells in current Climate Space")
legend("topleft", legend=unique(LUT$BIOME_NAME), cex=0.75, fill=as.character(unique(LUT$biome_color)))
dev.off()

#plot
pdf("current_ecorgn_climate_space_masked.pdf", 12, 8)
plot(now_sub.proj$PC1, now_sub.proj$PC2, col=as.character(now_sub.proj$color), pch=16, xlab="PC1 (approx. low to high energy)",
     ylab="PC2 (approx low to high water)", main="Current Ecoregions in Climate Space")
dev.off()




#for 2C condition

########################

