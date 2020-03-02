#### Analysis of changes in global ecoregions using reverse analogs ###

## Author: Clark Hollenberg
## Date: 2/3/20

library(rgdal)
library(raster)
library(rasterVis)
library(rgeos)
library(RColorBrewer)
library(maps)
library(ggplot2)
library(ncdf4)
library(geosphere)
library(IDPmisc)
library(sp)
library(dplyr)
# library(sf)
# library(scales)
# library(broom)
# library(fasterize)
setwd("~/ClimateAnalogs/analysis")  #laptop
setwd("~/Documents/Analogs") #desktop
load(".RData")

#if on original machine - load .RData from climateanalogs/analysis
##################################################################
#Pre-processing
########################################################################################################################
# read in ecoregion shapefile from 2017 paper to get lookup table -skip and load LUT.csv
####################################################
ecorgns<-st_read(dsn="Ecoregions2017shp",layer='Ecoregions2017')


### Look up table (LUT) between econames and ECO_ID
LUT<-data.frame(cbind(as.character(ecorgns$ECO_NAME), as.numeric(as.character(ecorgns$ECO_ID)), as.numeric(as.character(ecorgns$BIOME_NUM)), as.character(ecorgns$BIOME_NAME),
              as.character(ecorgns$COLOR), as.character(ecorgns$COLOR_BIO)), stringsAsFactors = FALSE)
names(LUT)<-c("econame","ECO_ID", "BIOME_ID", "BIOME_NAME", "color", "biome_color")

##this code is to order the LUT by ECO_ID, so the colors will map for plotting later on
#ECO_IDs from 0 to 846
dummy<-data.frame(0:846)
colnames(dummy)<- "ECO_ID"
LUT<-merge(dummy, LUT, by="ECO_ID")
rm(ecorgns, dummy)
write.csv(LUT, "LUT.csv")


#convert polygons to raster (3 tiny ecoregions are lost in this step)
# ecorgns_faster_now<-fasterize(ecorgns_sf, ecorgn_rast_now, field='ECO_ID')
# writeRaster(ecorgns_faster_now,"ecoregion_raster_current_ver2.tif",format="GTiff", overwrite=T)
# ecorgn_rast_now<-raster('ecoregion_raster_current_ver2.tif')

#Check to see which eco_ids were not rasterized
# temp<-data.frame('ECO_ID'=unique(ecorgns_faster_now))
# temp2<-data.frame(LUT$ECO_ID[!(LUT$ECO_ID %in% temp$ECO_ID)])
# colnames(temp2)<-'ECO_ID'
# temp<-merge(temp2, LUT, by='ECO_ID')

# read in sigma data to evaluate no analog locations
##################################################

#sigma raster whitespace exploration##
#############
sigma2<- brick("analog2c_sigma.nc")
sigma4<- brick("analog4c_sigma.nc")
sigma2<-raster(sigma2, layer = 1)
sigma4<-raster(sigma4, layer = 1)

#raster of locations with sigma>0.2  (for no analog with x10 scaling factor)
temp<-sigma2
temp[temp>0.2]=1
temp[temp<0.2]=0
tempy<-raster(sigma4, layer = 1)
tempy[tempy>0.2]=1
tempy[tempy<0.2]=0
pdf('No_analogs.pdf', 22, 8)
par(mfrow = c(1, 2))
plot(temp, legend = F, col=c('light gray', 'red'), main = "+2C Locations with sigma > 2 (No analog)")
plot(tempy, legend = F, col=c('light gray', 'red'), main = "+4C Locations with sigma > 2 (No analog)")
dev.off()

temp<-raster(sigma4, layer = 1)
temp[temp>0.2]=1
temp[temp<0.2]=0
pdf('No_analog_4C.pdf', 16, 8)
plot(temp, legend = F, col=c('gray96', 'red'), main = "+4C Locations with sigma > 2 (No analog)")
dev.off()

#convert NA values in sigma (with sigma>2) to binary raster
temp<-raster(sigma, layer = 1)
sigma_NA_bin<-temp
sigma_NA_bin[is.na(sigma_NA_bin[])]<-1
sigma_NA_bin[sigma_NA_bin[]!=1]<-0
#return oceans to NA
sigma_NA_bin[is.na(ecorgn_rast_now[])]<-NA

#plot no analog locations
pdf("NA_sigma_global.pdf", 20, 8)
par(mfrow=c(1,2))
plot(temp, main="Sigma Layer=1")
plot(sigma_NA_bin, col=c("light gray", "red"), main="+2C NA sigma values", legend=FALSE)
dev.off()

##compare NA sigma values with ecoregion holes in +2C
sigmaNA_compare_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
  bounds<-extent(extentVector)
  rast_2C<-crop(ecorgn_rast_2C, bounds)
  rast_sigma_NA<-crop(sigma_NA_bin, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 20, 8)
  par(mfrow = c(1, 3))
  plotLegend(rast_2C, NULL, leg_txt, leg_col)
  plotWColors(rast_2C, "+2C Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  plot(rast_sigma_NA, col=c("light gray", "red"), main="+2C NA sigma values", legend=FALSE)
  plot(rast_borders, bg="transparent", add=TRUE)
  #plot images
  dev.off()
}

sigmaNA_compare_PDF(c(-125, -105, 32, 49), "westrnUSA_sigmaNA.pdf", 1.1, 2, uSAstatelines)
sigmaNA_compare_PDF(c(-170, -140, 55, 75), "alaska_sigmaNA.pdf", 1.3, 1, nationalBorders)
sigmaNA_compare_PDF(c(-70, -53, -15, 5), "amazon_sigmaNA.pdf", 1.2, 2, nationalBorders)
sigmaNA_compare_PDF(c(-9, 20, 37, 65), "europe_sigmaNA.pdf", 1.1, 1, nationalBorders)

#############################



##############

### create rasters showing insufficient data vs no analog
################################################################


##load +2C rasterfile
setwd("~/Documents/Analogs/InRasters")
##from online datasource ecoregions2017.appspot.com, lost some elements through rasterization
ecorgn_rast_now<-raster("ecorast_now.tif") 
ecorgn_rast_2C<- raster("ecorast_2C.tif")
ecorgn_rast_4C<-raster("ecorast_4C.tif")

##mask in black No Analog locations from rasters (where sigma >2 [0.2 in layer]) and where we had sufficient data
ecorast_2C_mapped<-ecorgn_rast_2C
ecorast_4C_mapped<-ecorgn_rast_4C
ecorast_2C_mapped[is.na(ecorast_2C_mapped) & jmask == 2] <-847  #these are locations where there was enough data, but no analog could be found
ecorast_4C_mapped[is.na(ecorast_4C_mapped) & jmask == 2] <-847

#Mask in NA Values not over water as 848 (data issues)
ecorast_2C_mapped[jmask==1] <- 848
ecorast_2C_mapped[jmask==0] <- NA #these should already be NA
ecorast_4C_mapped[jmask==1] <- 848
ecorast_4C_mapped[jmask==0] <- NA

ecorast_now_mapped<-ecorgn_rast_now  #mask areas of insufficient data in the current (to allow for more direct comparison)
ecorast_now_mapped[jmask==1] <- 848
writeRaster(ecorast_now_mapped, "InRasters/ecorast_now_mapped.tif", overwrite=T)

#write these rasters out (including the masked sigma values)
setwd("~/Documents/Analogs/InRasters")
writeRaster(ecorast_2C_mapped, "ecorast_2C_mapped.tif", overwrite=T)
writeRaster(ecorast_4C_mapped, "ecorast_4C_mapped.tif", overwrite=T)

#change LUT to include No Analogs (eco_id=847)
LUT<- read.csv(file="LUT.csv", stringsAsFactors = F)
LUT[,'X'] <- NULL
LUT[1, 'BIOME_NAME'] <- "Rock and Ice"
LUT[1, 'BIOME_ID'] <- 15
LUT[nrow(LUT) + 1,] = c(847,"No analog", 16, "No analog", "#000000", "#000000")
LUT[nrow(LUT) + 1,] = c(848,"Insufficient data", 17, "Insufficient data", "#FFFFFF", "#FFFFFF")
write.csv(LUT, "LUT_plus.csv")




###############################################################################################################
#Loading input files
######################
ecorgn_rast_now<-raster("InRasters/ecorast_now.tif") 
ecorgn_rast_2C<- raster("InRasters/ecorast_2C.tif")
ecorgn_rast_4C<-raster("InRasters/ecorast_4C.tif")
ecorast_now_mapped<-raster("InRasters/ecorast_now_mapped.tif")
ecorast_2C_mapped<-raster("InRasters/ecorast_2C_mapped.tif")
ecorast_4C_mapped<-raster("InRasters/ecorast_4C_mapped.tif")
#load majority percentage from 100 layer sigma voting
votecount_rast_2C<-raster("ecorast_2C_votecount.tif")
votecount_rast_4C<-raster("ecorast_4C_votecount.tif")
##Abatzaglou mask for regions of insufficient data
##################
jmask<- raster("InRasters/johnmask.nc")
LUT<-read.csv("InRasters/LUT.csv", stringsAsFactors = F, encoding ='utf-8')
LUT_plus<-read.csv("InRasters/LUT_plus.csv", stringsAsFactors = F, encoding ='utf-8')
################################

#working with majority vote%
###########################
#average number of votes by ecoregion
meanVotePerc<-function(rasterName)
{
  vec<-vector()
  for (i in unique(rasterName))
  {
    temp<-votecount_rast_2C
    temp[rasterName != i]<-NA
    tempy<-cellStats(temp, stat='mean', na.rm=TRUE)
    vec<-c(vec, tempy)
    print(i)
  }
  out<-data.frame('ECO_ID' = unique(rasterName), 'Mean_vote_perc' = vec)
  return(out)
}
voteMeans_2C<-meanVotePerc(ecorgn_rast_2C)
voteMeans_4C<-meanVotePerc(ecorgn_rast_4C)

write.csv(voteMeans_2C, "Outputs/voteMeans_2C.csv", row.names = F)
write.csv(voteMeans_4C, "Outputs/voteMeans_4C.csv", row.names = F)
voteMeans_2C<-read.csv("Outputs/voteMeans_2C.csv")
voteMeans_4C<-read.csv("Outputs/voteMeans_4C.csv")

ecorgn_vote_compareMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
  bounds<-extent(extentVector)
  rast_current<-crop(ecorgn_rast_now, bounds)
  rast_2C<-crop(ecorgn_rast_2C, bounds)
  rast_vote<-crop(maj_vote_rast, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 26, 8)
  par(mfrow = c(1, 4), mar=c(5, 4, 4, 4))
  plotLegend(rast_current, rast_2C, leg_txt, leg_col)
  plotWColors(rast_current, "Current Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  plotWColors(rast_2C, "+2C Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  co<-colorRampPalette(c("red", "gray96", "blue"))(20)
  plot(rast_vote, col = co, main='% of Majority Vote')
  plot(rast_borders, bg="transparent", add=TRUE)
  #plot images
  dev.off()
}

#comparing maps of % majority vote
ecorgn_vote_compareMap_PDF(c(-125, -105, 32, 49), 'westrnUSA_votingMap.pdf', 1.15, 2, uSAstatelines)
ecorgn_vote_compareMap_PDF(c(-170, -140, 55, 75), "alaska_votingMap.pdf", 1.3, 1, nationalBorders)
ecorgn_vote_compareMap_PDF(c(-70, -53, -15, 5), "amazon_votingMap.pdf", 1.2, 2, nationalBorders)
ecorgn_vote_compareMap_PDF(c(-9, 20, 37, 65), "europe_votingMap.pdf", 1.1, 1, nationalBorders)
#######

#comparison perc protected xy plot
#####

pdf(file="Perc_protected_comparison_graph.pdf", 12, 10)
vec <- vector()
for (i in 1:nrow(voteMeans))
{
  temp<-voteMeans$Mean_vote_perc[i]*100
  temp<-as.integer(temp)
  #choose the color for the ecoregion based on the value of the average % vote
  vec <- c(vec, colorRampPalette(c('red', 'gray', 'blue'))(100)[temp])
}

plot(perc_compare_df$PA_perc_current, perc_compare_df$PA_perc_2C, xlim= c(0, 1), ylim= c(0, 1), xlab = 'Current proportion protected', 
     ylab= '+2C proportion protected', col=vec, main='Current vs +2C %Protected by Ecoregion')
abline(0, 1, lty = 'dashed')
legend("topleft", title = "% vote mean", legend = seq(from=1, to=0, by = -0.1), fill=colorRampPalette(c('blue', 'gray', 'red'))(11))
dev.off()
#####

#by biome
#####
pdf(file="Perc_protected_comparison_graph_biome.pdf", 20, 8)
par(mfrow=c(1,3))
for (i in 1:14)
{
  vec <- vector()
  df<-subset(perc_compare_df, BIOME_ID==i)
  for (i in 1:nrow(df))
  {
    temp<-df$Mean_vote_perc[i]*100
    temp<-as.integer(temp)
    #choose the color for the ecoregion based on the value of the average % vote
    vec <- c(vec, colorRampPalette(c('red', 'gray', 'blue'))(100)[temp])
  }
  plot(df$PA_perc_current, df$PA_perc_2C, xlim= c(0, 1), ylim= c(0, 1), xlab = 'Current proportion protected', 
       ylab= '+2C proportion protected', col = vec, main=unique(df$BIOME_NAME))
  legend("topleft", title = "% vote mean", legend = seq(from=1, to=0, by = -0.1), fill=colorRampPalette(c('blue', 'gray', 'red'))(11))
  abline(0, 1, lty = 'dashed')
}
dev.off()
#####

#read in Dinerstein to compare
#############
Dinerstein_eco<-read.csv("Dinerstein_percents.csv", header=FALSE)
colnames(Dinerstein_eco)<-c("ECO_ID", "econame", "total_area", "PA_area")
Dinerstein_eco$PA_perc_din<-round(Dinerstein_eco$PA_area/Dinerstein_eco$total_area*100, digits=1)

Dinerstein_eco<-merge(Dinerstein_eco, perc_current_df, by = c('ECO_ID', 'econame'))
Dinerstein_eco$Din_perc_compare<-Dinerstein_eco$PA_perc_din-(round(Dinerstein_eco$PA_perc_current*100, digits=1))

# xy plot comparing our % protected to the paper's
pdf("Dinerstein_comparison_graph.pdf")
plot(Dinerstein_eco$PA_perc_din, Dinerstein_eco$PA_perc_current*100, xlab="% protected Dinerstein", ylab="% protected current", main="Discrepancy with Dinerstein %Protected Current")
abline(0, 1, lty = 'dashed')
dev.off()

#show where large differences are in current % protected are
extremeDiff<-subset(Dinerstein_eco, Din_perc_compare>30)
temp_rast<-ecorgn_rast_now
temp_rast[temp_rast %in% extremeDiff$ECO_ID]<-1000000

extremeDiff<-subset(Dinerstein_eco, Din_perc_compare< -15)
temp_rast2<-ecorgn_rast_now
temp_rast2[temp_rast2 %in% extremeDiff$ECO_ID]<-1000000

pdf('Areas_w_dinerstein_diff_protected.pdf', 18, 8)
par(mfrow=c(1,2))
plot(temp_rast, legend=FALSE, main="Regions of discrepancy >30% protected Dinerstein")
plot(temp_rast2, legend=FALSE, main="Regions of discrepancy < -15% protected Dinerstein")
dev.off()



