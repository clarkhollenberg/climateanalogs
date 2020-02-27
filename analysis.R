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
# library(sf)
# library(scales)
# library(broom)
# library(fasterize)
setwd("~/ClimateAnalogs/analysis")  #laptop
setwd("~/Documents/Analogs") #desktop

#if on original machine - load .RData from climateanalogs/analysis

# read in ecoregion shapefile from 2017 paper to get lookup table
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

##check Abatzaglou mask for NA reasons
##################
jmask<- raster("johnmask.nc")

##############

### read in ecoregion rasters and border shp files ##
################################################################


##load +2C rasterfile
setwd("~/Documents/Analogs/InRasters")
##from online datasource ecoregions2017.appspot.com, lost some elements through rasterization
ecorgn_rast_now<-raster("ecorast_current.tif") 
ecorgn_rast_2C<- raster("ecorast_2C.tif")
ecorgn_rast_4C<-raster("ecorast_4C.tif")

##mask in black No Analog locations from rasters (where sigma >2 [0.2 in layer]) and where we had sufficient data
ecorgn_rast_2C[is.na(ecorgn_rast_2C) & jmask == 2] <-847
ecorgn_rast_4C[is.na(ecorgn_rast_4C) & jmask == 2] <-847

#Mask in NA Values not over water as 848 (data issues)
ecorgn_rast_2C[jmask==1] <- 848
ecorgn_rast_2C[jmask==0] <- NA
ecorgn_rast_4C[jmask==1] <- 848
ecorgn_rast_4C[jmask==0] <- NA

#write these rasters out (including the masked sigma values)
setwd("~/Documents/Analogs/InRasters")
writeRaster(ecorgn_rast_2C, "ecorast_2C_mapped.tif", overwrite=T)
writeRaster(ecorgn_rast_4C, "ecorast_4C_mapped.tif", overwrite=T)

#change LUT to include No Analogs (eco_id=847)
LUT<- read.csv(file="LUT.csv", stringsAsFactors = F)
LUT[,'X'] <- NULL
LUT[1, 'BIOME_NAME'] <- "Rock and Ice"
LUT[1, 'BIOME_ID'] <- 15
LUT[nrow(LUT) + 1,] = c(847,"No analog", 16, "No analog", "#000000", "#000000")
LUT[nrow(LUT) + 1,] = c(848,"Insufficient data", 17, "Insufficient data", "#FFFFFF", "#FFFFFF")
write.csv(LUT, "LUT_plus.csv")

#load majority percentage from 100 layer sigma voting
maj_vote_rast<-raster("ecorgn_percentmaj2C.tif")
#change values pushed to NA on the landsurface for which there was a climate analog to 1 (Solomon masked these out)
temp<-maj_vote_rast
temp[is.na(temp)]<-1
temp[is.na(ecorgn_rast_2C)]<-NA
maj_vote_rast<-temp

##load countrylines for mapping
nationalBorders<-readOGR(dsn="countries_shp",layer='countries')
##load statelines for mapping
uSAstatelines<-readOGR(dsn="USA_state_shp",layer='cb_2018_us_state_5m')
###############################################################

#plotting ecoregion comparisons
###################


#function takes raster input and adjusts colors to match eco ids
plotWColors<-function(rasterName, title, leg_x=NULL, leg_y=NULL, txtsize=NULL)
{
  df <- data.frame(unique(rasterName))
  colnames(df)<- "ECO_ID"
  df<-merge(df, LUT, by="ECO_ID")
  color_palate_leg <- as.character(df$color)
  legend_names <- as.character(df$econame)
  
  rangeValues<-c((minValue(rasterName)+1):(maxValue(rasterName)+1))
  color_palate_plot<-as.character(LUT$color[rangeValues])
  plot(rasterName, col=color_palate_plot, main = title, legend=FALSE)
}

#function takes raster input and adjusts colors to match biome ids
plotByBiome<-function(rasterName, title)
{
  rangeValues<-c((minValue(rasterName)+1):(maxValue(rasterName)+1))
  color_palate_plot<-as.character(LUT$biome_color[rangeValues])
  plot(rasterName, col=color_palate_plot, main = title, legend=FALSE)
}


#function plots seperate color coded legend, showing ecoregions in two raster maps being compared
plotLegend<-function(rasterName1, rasterName2, rasterName3, txtsize, columns)
{
  #combine unique ecoregions from the two rasters to create a legend that represents all of them
  df1<-unique(rasterName1)
  df2<-unique(rasterName2)
  df3<-unique(rasterName3)
  df<-data.frame(unique(c(df1, df2, df3)))
              
  colnames(df)<- "ECO_ID"
  df<-merge(df, LUT, by="ECO_ID")
  color_palate_leg <- as.character(df$color)
  legend_names <- as.character(df$econame)
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend=legend_names, cex=txtsize, ncol = columns, fill=color_palate_leg)
}

#function plots seperate color coded legend, showing ecoregions in two raster maps being compared
plotBiomeLegend<-function(rasterName1, rasterName2, rasterName3, txtsize, columns)
{
  #combine unique ecoregions from the three rasters to create a legend that represents all of them
  df1<-unique(rasterName1)
  df2<-unique(rasterName2)
  df3<-unique(rasterName3)
  df<-data.frame(unique(c(df1, df2, df3)))
  colnames(df)<- "ECO_ID"
  df<-merge(df, LUT, by="ECO_ID")
  #pull unique biomes from these ecoregions and create new LUT
  df1<-data.frame(unique(df$BIOME_ID))
  colnames(df1)<- "BIOME_ID"
  df<-merge(df1, LUT, by="BIOME_ID")
  #now we have the correct biomes, but need only one row per biome
  color_palate_leg <- as.character(unique(df$biome_color))
  legend_names <- as.character(unique(df$BIOME_NAME))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend=legend_names, cex=txtsize, ncol = columns, fill=color_palate_leg)
}

#function to make figures as pdf comparing two ecoregion maps with defined boundary
ecorgn_compareMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
  bounds<-extent(extentVector)
  rast_current<-crop(ecorgn_rast_now, bounds)
  rast_2C<-crop(ecorgn_rast_2C, bounds)
  rast_4C<-crop(ecorgn_rast_4C, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 28, 8)
  par(mfrow = c(1, 4))
  plotLegend(rast_current, rast_2C, rast_4C, leg_txt, leg_col)
  plotWColors(rast_current, "Current Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  plotWColors(rast_2C, "+2C Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  plotWColors(rast_4C, "+4C Ecoregions")
  plot(rast_borders, bg="transparent", add=TRUE)
  #plot images
  dev.off()
}

#function to plot biome comparison
biome_compareMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
  bounds<-extent(extentVector)
  rast_current<-crop(ecorgn_rast_now, bounds)
  rast_2C<-crop(ecorgn_rast_2C, bounds)
  rast_4C<-crop(ecorgn_rast_4C, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 25, 8)
  par(mfrow = c(1, 4))
  plotBiomeLegend(rast_current, rast_2C, rast_4C, leg_txt, leg_col)
  plotByBiome(rast_current, "Current Biomes")
  plot(rast_borders, bg="transparent", add=TRUE)
  plotByBiome(rast_2C, "+2C Biomes")
  plot(rast_borders, bg="transparent", add=TRUE)
  plotByBiome(rast_4C, "+4C Biomes")
  plot(rast_borders, bg="transparent", add=TRUE)
  #plot images
  dev.off()
}

#function to make figues as pdf showing movement vectors of ecoregions from now to 2C
create_ecorgn_movementMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
  bounds<-extent(extentVector)
  rast_current<-crop(ecorgn_rast_now, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 18, 10)
  par(mfrow=c(1,2))
  
  # layout(mat=matrix(c(1, 2), nrow=1, ncol=2), heights=c(1), widths=c(1, 2))
  plotLegend(rast_current, NULL, leg_txt, leg_col)
  plotWColors(rast_current, "Ecoregion Movement to +2C Locations")
  # layout(mat=matrix(c(1, 2), nrow=1, ncol=2), heights=c(1), widths=c(1, 2))
  plot(rast_borders, bg="transparent", add=TRUE)
  drawDispVectors(rast_current, extentVector)
  #plot images
  dev.off()
} 

#function to draw vectors between ecoids on selected raster area
drawDispVectors<-function(rasterName, extentVector)
{
  df<-data.frame(unique(rasterName))
  colnames(df)<-"ECO_ID"
  df<-merge(df, centr_mastr, by="ECO_ID")
  for (i in unique(rasterName))
  {
    #we can only draw an arrow if the ecoregion exists in both current and 2c conditions, there are fewer ecoregions in +2C
    if (i %in% df$ECO_ID)
    {
      df2<-subset(df, ECO_ID==i)
      x1<-as.numeric(df2$x_now)
      x2<-as.numeric(df2$x_2C)
      y1<-as.numeric(df2$y_now)
      y2<-as.numeric(df2$y_2C)
      
      #only plot the arrow if it is within the raster bounds
      if (extentVector[1]<x1 & x1<extentVector[2] & extentVector[3]<y1 & y1<extentVector[4])
      {
        Arrows(x1, y1, x2, y2, size=0.5)
      }
    }
  }
}

#applying the functions to generate ecorgn_comparison plots
ecorgn_compareMap_PDF(c(-125, -105, 32, 49), "westrnUSA_ecorgns.pdf", 1.1, 2, uSAstatelines)
ecorgn_compareMap_PDF(c(-170, -140, 55, 75), "alaska_ecorgns.pdf", 1, 2, nationalBorders)
ecorgn_compareMap_PDF(c(-70, -53, -15, 5), "amazon_ecorgns.pdf", 1, 2, nationalBorders)
ecorgn_compareMap_PDF(c(-9, 20, 37, 65), "europe_ecorgns.pdf", 0.9, 1, nationalBorders)
ecorgn_compareMap_PDF(c(80, 110, 25, 45), "asia_ecorgns.pdf", 0.8, 2, nationalBorders)
ecorgn_compareMap_PDF(c(10, 40, -10, 25), "congo_ecorgns.pdf", 0.9, 2, nationalBorders)

#applying for biome comparison plots
biome_compareMap_PDF(c(-125, -105, 32, 49), "westrnUSA_biomes.pdf", 2, 1, uSAstatelines)
biome_compareMap_PDF(c(-170, -140, 55, 75), "alaska_biomes.pdf", 2, 1, nationalBorders)
biome_compareMap_PDF(c(-70, -53, -15, 5), "amazon_biomes.pdf", 1.5, 1, nationalBorders)
biome_compareMap_PDF(c(-9, 20, 37, 65), "europe_biomes.pdf", 2, 1, nationalBorders)
biome_compareMap_PDF(c(80, 110, 25, 45), "asia_biomes.pdf", 1, 1, nationalBorders)
biome_compareMap_PDF(c(10, 40, -10, 25), "congo_biomes.pdf", 1.1, 1, nationalBorders)

#maps with arrows from current to 2C
create_ecorgn_movementMap_PDF(c(-125, -105, 32, 49), 'westrnUSA_ecorgn_movement.pdf', 1.15, 1, uSAstatelines)
create_ecorgn_movementMap_PDF(c(-170, -140, 55, 75), 'alaska_ecorgns_movement.pdf', 2, 1, nationalBorders)
create_ecorgn_movementMap_PDF(c(-70, -52, -15, 5), "amazon_ecorgns_movement.pdf", 1.15, 1, nationalBorders)
create_ecorgn_movementMap_PDF(c(-9, 20, 37, 65), "europe_ecorgns_movement.pdf", 1.0, 1, nationalBorders)


################################

#working with majority vote%
###########################

meanVotePerc<-function()
{
  vec<-vector()
  for (i in unique(ecorgn_rast_2C))
  {
    temp<-maj_vote_rast
    temp[ecorgn_rast_2C != i]<-NA
    tempy<-cellStats(temp, stat='mean', na.rm=TRUE)
    vec<-c(vec, tempy)
    print(i)
  }
  out<-data.frame('ECO_ID' = unique(ecorgn_rast_2C), 'Mean_vote_perc' = vec)
  return(out)
}
voteMeans<-meanVotePerc()



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

#determine where ecoregions are changing globally (where the ECO_ID changes between rasters)
#######################################################
ecorgn_rast_change <- ecorgn_rast_2C - ecorgn_rast_now

###Maps of ecoregion shift as binary
#####
ecorgn_rast_change_bin<-ecorgn_rast_change
ecorgn_rast_change_bin[ecorgn_rast_change_bin[]>0 | ecorgn_rast_change_bin[]<0]<- 1
rm(ecorgn_rast_change)

#create map of binary ecoregion change, overlaid with protected areas
areas_ecoregion_change_PDF<-function(extentVector, filename, leg_txt, borderShp)
{
  bounds<-extent(extentVector)
  #Use ecorgn_rast_now to get a gray background for the land surface (ensuring that NA data on the land isn't plotted as white)
  rast_bg<-crop(sigma_NA_bin, bounds)
  rast_change<-crop(ecorgn_rast_change_bin, bounds)
  rast_PA<-crop(PA_bin, bounds)
  rast_borders<-crop(borderShp, bounds)
  
  pdf(file= filename, 18, 10)
  par(mfrow=c(1,2))
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("topleft", legend=c("Ecoregion static", "Ecoregion shift", "Protected areas", "Protected Areas with shifting ecoregion"), cex=leg_txt, 
         ncol = 1, fill=c("light grey", "red", "yellow", "orange"))
  plot(rast_bg, col=c("light grey", "light grey"), main = "Areas of ecoregion change +2C", legend=FALSE)
  plot(rast_change, col=c("light grey", "red"),
        legend=FALSE, add=TRUE)
  plot(rast_PA, col=alpha("yellow", 0.6), legend=FALSE, add=TRUE)
  plot(rast_borders, bg="transparent", add=TRUE)
  #plot images
  dev.off()
}  
areas_ecoregion_change_PDF(c(-125, -105, 32, 49), 'westrnUSA_ecorgn_change.pdf', 2, uSAstatelines)
areas_ecoregion_change_PDF(c(-170, -140, 55, 75), 'alaska_ecorgns_change.pdf', 2, nationalBorders)
areas_ecoregion_change_PDF(c(-70, -52, -15, 5), "amazon_ecorgns_change.pdf", 2, nationalBorders)
areas_ecoregion_change_PDF(c(-9, 20, 37, 65), "europe_ecorgns_change.pdf", 2, nationalBorders)

#global plot ecoregion shift
#################
pdf(file="global_ecorgns_change.pdf", 18, 10)
plot(sigma_NA_bin, col=c("light grey", "light grey"), main = "Areas of ecoregion change +2C", legend=FALSE)
legend("topleft", legend=c("Ecoregion static", "Ecoregion shift", "Protected areas", "Protected Areas with shift"), cex=1, 
       ncol = 1, fill=c("light grey", "red", "yellow", "orange"))
plot(ecorgn_rast_change_bin, col=c("light grey", "red"),
     legend=FALSE, add=TRUE)
plot(PA_bin, col=alpha("yellow", 0.6), legend=FALSE, add=TRUE)
# plot(nationalBorders, bg="transparent", add=TRUE)
#plot images
dev.off()
#####################################

#analyze percent protected changes
##############################################

PA<-raster("~/ClimateAnalogs/analysis/PA/PA.IVInoass.r.GTE75perc_2019-12-23.tif")

# convert to binary image
PA_bin <- PA
PA_bin[PA>0]<-1 
rm(PA)

analyzeByPerc<- function(ecorgn_rast, nameEnd)
{
  # multiply PA binary by ecoregion raster
  PA_eco<-PA_bin*ecorgn_rast  # returns ECO_ID in PAs
  length(unique(PA_eco)) ## 694 with values 
  
  ### sum area by unique levels of ecoregion
  PA_area<-tapply(area(PA_eco), PA_eco[], sum)   # 694 unique values
  
  temp<-as.numeric(PA_area)
  #sums ecoregion areas in PAs
  PA_area_table<-data.frame(ECO_ID=rownames(PA_area),PA_area=temp)
  
  
  ### merge LUT and area table, note how all ECO_IDs are retained with all=TRUE
  PA_area_table<-merge(LUT, PA_area_table,by="ECO_ID", all=TRUE)
  PA_area_table$PA_area[is.na(PA_area_table$PA_area)]<-0
  
  
  # sums total ecoregion area, don't include ecoregions w no area to avoid divide by 0
  ecorgn_area<-tapply(area(ecorgn_rast), ecorgn_rast[], sum)
  temp<-as.numeric(ecorgn_area)
  total_ecoregn_area_tbl<-data.frame(ECO_ID=rownames(ecorgn_area),tot_area=temp)
  
  ## merge total area with PA data
  
  final_table<-merge(PA_area_table, total_ecoregn_area_tbl, by="ECO_ID")
  final_table$color<-NULL
  final_table$biome_color<-NULL
  # and calculate percent of area that is protected
  final_table$PA_percent<-final_table$PA_area/final_table$tot_area
  colnames(final_table)[5:7]<-c(paste('PA_area', nameEnd, sep="_"), paste('tot_area', nameEnd, sep="_"), paste('PA_perc', nameEnd, sep="_"))
  # colnames(final_table)['tot_area']<-paste('tot_area', nameEnd, sep="_")
  # colnames(final_table)['PA_percent']<-paste('tot_area', nameEnd, sep="_")
  return(final_table)
}

#creating dataframes to look at PA % changes
perc_current_df<-analyzeByPerc(ecorgn_rast_now, 'current')
perc_2C_df<-analyzeByPerc(ecorgn_rast_2C, '2C')
perc_4C_df<-analyzeByPerc(ecorgn_rast_4C, '4C')
perc_compare_df<-merge(perc_current_df, perc_2C_df, by=c("ECO_ID", 'econame', 'BIOME_ID', 'BIOME_NAME'))
perc_compare_df<-merge(perc_compare_df, perc_4C_df, by=c("ECO_ID", 'econame', 'BIOME_ID', 'BIOME_NAME'))
perc_compare_df$PA_perc_change_2C<-perc_compare_df$PA_perc_2C - perc_compare_df$PA_perc_current
perc_compare_df$PA_perc_change_4C<-perc_compare_df$PA_perc_4C - perc_compare_df$PA_perc_current
perc_compare_df<-merge(perc_compare_df, voteMeans, by='ECO_ID')
write.csv(perc_compare_df, file='PA_perc_comparison_w4C.csv')



perc_current_rast<-subs(ecorgn_rast_now, perc_current_df, by="ECO_ID",which="PA_perc_current")
perc_2C_rast<-subs(ecorgn_rast_2C, perc_2C_df, by="ECO_ID",which="PA_perc_2C")
perc_4C_rast<-subs(ecorgn_rast_4C, perc_4C_df, by="ECO_ID",which="PA_perc_4C")
perc_change_rast_2C<-perc_2C_rast - perc_current_rast
perc_change_rast_4C<-perc_4C_rast - perc_current_rast

ave_percByBiome<-function(datasource, colnametot, colnamePA)
{
 vec<-vector()
 for (i in 1:14)
 {
   df<-subset(datasource, BIOME_ID==i)
   temp<-sum(as.numeric(df[,colnametot]))
   temp2<-sum(as.numeric(df[,colnamePA]))
   #double check that this is the correct column name in the datasource before running this
   vec<-c(vec, temp2/temp)
 }
 return(vec)
}


#plot comparison perc protected
#####
pdf(file="Perc_protected.pdf", 20, 8)
par(mfrow=c(1,2))
co<-colorRampPalette(c("red", "gray96", "blue"), bias=3)(25)
plot(perc_current_rast, col=co, main="Proportion of current ecoregions protected")
plot(nationalBorders, add=TRUE, legend=FALSE)
plot(perc_2C_rast, col=co, main="Proportion of +2C ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
dev.off()
#####

#plot change in perc protected
#####
pdf(file="Perc_protected_change.pdf", 20, 8)
par(mfrow=c(1, 2))
co<-colorRampPalette(c("red", "gray96", "blue"))(20)
plot(perc_change_rast_2C, col=co, xlim= c(-180, 180), zlim=c(-0.5, 0.5), main="+2C Change in proportion of ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
plot(perc_change_rast_4C, col=co, xlim= c(-180, 180), zlim=c(-0.5, 0.5), main="+4C Change in proportion of ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
dev.off()
#####

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

#####################################

######Calculate centroids for ecoregion##
##################
centroidCalc<-function(ecorgn_rast, filename)
{
  for (i in unique(ecorgn_rast))
  {
    
    xy_i<-colMeans(xyFromCell(ecorgn_rast, which(ecorgn_rast[]==i)))
    if (exists("centr"))
      {
        centr <- rbind(centr,xy_i)
      }
    else
      {
        centr <- xy_i
      }
    print(i)
    
  }
  centr <- cbind(unique(ecorgn_rast), centr)
  cent <- data.frame(centr)
  rownames(centr) <- c()
  colnames(centr) <- c("ECO_ID", "x", "y")
  write.csv(centr, filename)
  return(centr)
}

centroidCalcBiome<-function(ecorgn_rast, filename)
{
  
  for (i in 1:14)
  {
    
    ecobioVector<-subset(sorted_LUT, sorted_LUT$BIOME_ID==i, select=ECO_ID)
    xy_i<-data.frame(xyFromCell(ecorgn_rast, which(ecorgn_rast[] %in% ecobioVector$ECO_ID)))
    xy_N<-subset(xy_i, y>0)
    xy_N<-colMeans(xy_N)
    xy_S<-subset(xy_i, y<0)
    xy_S<-colMeans(xy_S)
  
    if (exists("centr"))
    {
      centr <- rbind(centr, cbind(i, xy_N[1], xy_N[2], xy_S[1], xy_S[2]))
    }
    else
    {
      centr <- cbind(i, xy_N[1], xy_N[2], xy_S[1], xy_S[2])
    }
    print(i)
    colnames(centr)<-c("BIOME_ID", "x_N", "y_N", "x_S", "y_S")
    
  }
  write.csv(centr, filename)
  return(centr)
}

centr_now <- centroidCalc(ecorgn_rast_now, "Current_ecoregion_centroids.csv")
centr_now <- data.frame(centr_now)
rownames(centr_now) <- c()
colnames(centr_now) <- c("ECO_ID", "x_now", "y_now")

# points(cbind(centr_now[,2], centr_now[,3]))
centr_Biome_now<-centroidCalcBiome(ecorgn_rast_now, "biome_centroids_current.csv")
centr_Biome_2C<-centroidCalcBiome(ecorgn_rast_2C, "biome_centroids_2C.csv")
colnames(centr_Biome_now)<-c("ECO_ID", "x_N_now", "y_N_now", "x_S_now", "y_S_now")
colnames(centr_Biome_2C)<-c("ECO_ID", "x_N_2C", "y_N_2C", "x_S_2C", "y_S_2C")
centr_Biome_mastr<-merge(centr_Biome_2C, centr_Biome_now)
centr_Biome_mastr$displacement_km_N <-apply(centr_Biome_mastr, 1, function(k) measure_geodist(k['x_N_now'], k['y_N_now'], k['x_N_2C'], k['y_N_2C']))
centr_Biome_mastr$displacement_km_S <-apply(centr_Biome_mastr, 1, function(k) measure_geodist(k['x_S_now'], k['y_S_now'], k['x_S_2C'], k['y_S_2C']))
centr_Biome_mastr$bearing_N <-apply(centr_Biome_mastr, 1, function(k) measure_bearing(k['x_N_now'], k['y_N_now'], k['x_N_2C'], k['y_N_2C']))
centr_Biome_mastr$bearing_S <-apply(centr_Biome_mastr, 1, function(k) measure_bearing(k['x_S_now'], k['y_S_now'], k['x_S_2C'], k['y_S_2C']))

centr_2C <- centroidCalc(ecorgn_rast_2C, "Centroids_ecoregion_2C.csv")
colnames(centr_2C) <- c("ECO_ID", "x_2C", "y_2C")

centr_mastr <- merge(centr_now, centr_2C)
#remove rock and ice outlier
centr_mastr <- centr_mastr[-c(1),]
#create histogram of latitudinal movements
hist(centr_mastr$displacement_lat, breaks = 100, xlab="North/South Movement (km)", main = "Ecoregion Centroid Latitudinal Shifts")

pdf("Latitudinal Movement by Biome.pdf", paper="letter")
par(mfcol=c(3,1))
for (i in 1:14)
{
  temp<-subset(centr_mastr, BIOME_ID==i)
  hist(temp$displacement_lat_sign, breaks = 20, xlim=c(-1500, 1500), xlab="North/South Movement (km)", main = as.character(unique(temp$BIOME_NAME)))
}
dev.off()

measure_geodist <- function(lon_from, lat_from, lon_to, lat_to)
{
  dist <- round(distm(c(lon_from, lat_from), c(lon_to, lat_to), fun = distHaversine)/1000, digits=2)
  return(dist)
}

measure_latchange <- function(lat_from, lat_to)
{
  dist <- round(distm(c(0, lat_from), c(0, lat_to), fun = distHaversine)/1000, digits=2)
  return(dist)
}

measure_latchange_sign <- function(latChange, bearing)
{
  if ((as.numeric(bearing) < 90) | (as.numeric(bearing) > 270))
  {return(as.numeric(latChange))}
  else
  {return(as.numeric(latChange)*-1)}
}

measure_bearing <- function(lon_from, lat_from, lon_to, lat_to)
{
  if (is.nan(lon_from) | is.nan(lat_from) | is.nan(lon_to) | is.nan(lon_to))
  {
    return(NULL)
  }
  else
  {
    bearing <- round(bearingRhumb(c(lon_from, lat_from), c(lon_to, lat_to)), digits=2)
    return(bearing)
  }
  
}

#Create columns calculating centroid movement
centr_mastr$displacement_km <-apply(centr_mastr, 1, function(k) measure_geodist(k['x_now'], k['y_now'], k['x_2C'], k['y_2C']))
centr_mastr$bearing <-apply(centr_mastr, 1, function(k) measure_bearing(k['x_now'], k['y_now'], k['x_2C'], k['y_2C']))
#Looking at latitude shifts
centr_mastr$displacement_lat <-apply(centr_mastr, 1, function(k) measure_latchange(k['y_now'], k['y_2C']))
centr_mastr$displacement_lat_sign <-apply(centr_mastr, 1, function(k) measure_latchange_sign(k['displacement_lat'], k['bearing']))

##############

##create rose plot showing distance and direction of centroid movement##
#######################
pdf(file= "Rose plots by biome.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=df$BIOME_NAME[1])
  plot(centr_rose)
}
dev.off()

pdf(file= "Rose plots Northern.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  df<-subset(df, y_now>0)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=paste("N", df$BIOME_NAME[1]))
  plot(centr_rose)
}
dev.off()

pdf(file= "Rose plots Southern.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  df<-subset(df, y_now < 0)
  if (nrow(df)!=0)
  {
    centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=paste("S", df$BIOME_NAME[1]))
    plot(centr_rose)
  }
}
dev.off()

pdf(file= "Rose plots by biome aggr.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, fun=count)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=df$BIOME_NAME[1])
  plot(centr_rose)
}
dev.off()

############


#### summarize values by biome
########################################
library(vioplot)
table.2C<-read.csv("PA percent 2C.csv",header=T)
table.current<-read.csv("PA percent current.csv",header=T)
table.4C<-read.csv("PA percent 4C.csv",header=T)

## remove "rock and ice" ####
table.current<-table.current[!(table.current$econame=="Rock and Ice"),]
table.2C<-table.2C[!(table.2C$econame=="Rock and Ice"),]

### comparison of current against 2C
pdf("2C Violins PA protected proportion by biome.pdf", width=8,height = 7)
par(mar=c(5,18,1,1),bg=NA)

vioplot(table.current$PA.percent~table.current$biome, horizontal=T,las=2,ylab="",cex.axis=0.75,col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="proportion of area protected")
vioplot(table.2C$PA.percent~table.2C$biome, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
dev.off()


## change in area between biomes? ####
par(mar=c(5,18,1,1),bg=NA)
vioplot(table.current$PA.area~table.current$biome, ylog=F, horizontal=T,las=2,ylab="",cex.axis=0.75,col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="area")
vioplot(table.2C$PA.area~table.2C$biome, ylog=F, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)

### total biome area summed and compared ######
biome.area.current<-tapply(table.current$tot.area, table.current$biome, sum)
biome.area.2C<-tapply(table.2C$tot.area, table.2C$biome, sum)

biome.comp<-data.frame(biome=rownames(biome.area.current),area.current=as.numeric(biome.area.current),area.2C=as.numeric(biome.area.2C))

plot(biome.comp$area.current,biome.comp$area.2C,xlab=" area - current",ylab="area - +2C")
abline(a=0,b=1,lty=2)

text(area.2C ~area.current, pos=1,labels=biome,data=biome.comp, cex=0.5, font=2)


par(mar=c(5,18,1,1),bg=NA)
barplot(biome.area.comp$area.current,biome.area.comp$area.2C,horiz=T, names.arg=biome.area.comp$biome,las=2)

barplot(biome.area.comp$area.current,biome.area.comp$area.2C,horiz=T, names.arg=biome.area.comp$biome,las=2)

## comparions of current against 4C
par(mar=c(3,20,1,1),bg=NA)

vioplot(table.current$PA.percent~table.current$biome, horizontal=T,las=2,ylab="",cex.axis=0.75,col="NA",colMed="black",areaEqual=F)
vioplot(table.4C$PA.percent~table.4C$biome, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",border="blue",colMed="blue",areaEqual=F)


tapply(table.4C$PA.percent,table.4C$biome,median,na.rm=T)


biome.current<-tapply(table.current$PA.percent,table.current$biome,mean,na.rm=T)
temp<-as.numeric(biome.current)
current<-data.frame(biome=rownames(biome.current),percent=temp)
current<-na.omit(current)

# 2C data
biome.2C<-tapply(table.2C$PA.percent,table.2C$biome,mean,na.rm=T)
temp<-as.numeric(biome.2C)
dat.2C<-data.frame(biome=rownames(biome.2C),percent=temp)
dat.2C<-na.omit(dat.2C)

dotchart(sort(current$percent),labels=current$biome)
dotchart(sort(dat.2C$percent),labels=dat.2C$biome,add=T,col="blue")