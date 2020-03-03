#analyze percent protected changes
##############################################
setwd("Documents/Analogs")
load(".RData")
# PA<-raster("~/ClimateAnalogs/analysis/PA/PA.IVInoass.r.GTE75perc_2019-12-23.tif") #laptop
# PA<-raster("InRasters/PA_rast.tif") 
# # convert to binary image
# PA_bin <- PA
# PA_bin[PA>0]<-1 
# rm(PA)

PA_bin<-raster("InRasters/PA_bin.tif")

analyzeByPerc<- function(ecorgn_rast, nameEnd)
{
  # multiply PA binary by ecoregion raster
  PA_eco<-PA_bin*ecorgn_rast  # returns ECO_ID in PAs
  
  ### sum area by unique levels of ecoregion
  PA_area<-tapply(area(PA_eco), PA_eco[], sum)  
  
  #sums ecoregion areas in PAs
  PA_area_table<-data.frame(ECO_ID=rownames(PA_area), PA_area=as.numeric(PA_area))
  
  ### merge LUT and area table, note how all ECO_IDs are retained with all=TRUE
  PA_area_table<-merge(LUT_plus, PA_area_table,by="ECO_ID", all=TRUE)
  PA_area_table$PA_area[is.na(PA_area_table$PA_area)]<-0
  
  # sums total ecoregion area, don't include ecoregions w no area to avoid divide by 0
  ecorgn_area<-tapply(area(ecorgn_rast), ecorgn_rast[], sum)
  total_ecoregn_area_tbl<-data.frame(ECO_ID=rownames(ecorgn_area),tot_area=as.numeric(ecorgn_area))
  total_ecoregn_area_tbl$tot_area[is.na(total_ecoregn_area_tbl$tot_area)]<-0
  
  ## merge total area with PA data
  final_table<-merge(PA_area_table, total_ecoregn_area_tbl, by="ECO_ID", all=T)
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
############################################################
perc_current_df<-analyzeByPerc(ecorast_now_mapped, 'current')
perc_2C_df<-analyzeByPerc(ecorast_2C_mapped, '2C')
perc_4C_df<-analyzeByPerc(ecorast_4C_mapped, '4C')
perc_compare_df<-merge(perc_current_df, perc_2C_df, by=c("ECO_ID", 'econame', 'BIOME_ID', 'BIOME_NAME'), all=T)
perc_compare_df<-merge(perc_compare_df, perc_4C_df, by=c("ECO_ID", 'econame', 'BIOME_ID', 'BIOME_NAME'), all=T)
perc_compare_df[is.na(perc_compare_df)]<-0  #set NAs from merge to 0
perc_compare_df$PA_perc_change_2C<-perc_compare_df$PA_perc_2C - perc_compare_df$PA_perc_current
perc_compare_df$PA_perc_change_4C<-perc_compare_df$PA_perc_4C - perc_compare_df$PA_perc_current
#add in average votes for each ecoregion
perc_compare_df<-merge(perc_compare_df, voteMeans_2C, by='ECO_ID', all=T)
perc_compare_df<-merge(perc_compare_df, voteMeans_4C, by='ECO_ID', all=T)
write.csv(perc_compare_df, file='Outputs/PA_perc_comparison_master.csv', row.names=F)

#Summarize which biomes lost what area to no analog
#######################################################
analog_bin_2C<-ecorast_now_mapped
analog_bin_2C[ecorast_2C_mapped!=847]<-NA
noAnalogSum_2C<-tapply(area(analog_bin_2C), analog_bin_2C[], sum)
analog_bin_4C<-ecorast_now_mapped
analog_bin_4C[ecorast_4C_mapped!=847]<-NA
noAnalogSum_4C<-tapply(area(analog_bin_4C), analog_bin_4C[], sum)
noAnalogSum<-data.frame('ECO_ID'=rownames(noAnalogSum_2C), 'noAnalog_2C'=noAnalogSum_2C)
temp<-data.frame('ECO_ID'=rownames(noAnalogSum_4C), 'noAnalog_4C'=noAnalogSum_4C)
noAnalogSum<-merge(noAnalogSum, temp, by='ECO_ID', all=T)
noAnalogSum<-merge(LUT_plus, noAnalogSum, by='ECO_ID', all=T)
noAnalogSum[is.na(noAnalogSum)]<-0

#Create tables of total area by biome
################################################################
biomeArea.df<-as.data.frame(tapply(perc_compare_df$tot_area_current,perc_compare_df$BIOME_ID,FUN = sum))
biomeArea.df$Area_2C<-tapply(perc_compare_df$tot_area_2C,perc_compare_df$BIOME_ID,FUN = sum)
biomeArea.df$Area_4C<-tapply(perc_compare_df$tot_area_4C,perc_compare_df$BIOME_ID,FUN = sum)
biomeArea.df$noAnalog_2C<-tapply(noAnalogSum$noAnalog_2C,noAnalogSum$BIOME_ID,FUN = sum)
biomeArea.df$noAnalog_4C<-tapply(noAnalogSum$noAnalog_4C,noAnalogSum$BIOME_ID,FUN = sum)
colnames(biomeArea.df)<-c("Area_now", "Area_2C", "Area_4C", "noAnalog_2C", "noAnalog_4C")
biomeArea.df$BIOME_ID<-rownames(biomeArea.df)
LUT_biome<-data.frame("BIOME_ID"=LUT_plus$BIOME_ID, "BIOME_NAME"=LUT_plus$BIOME_NAME)
LUT_biome<-unique(LUT_biome)
biomeArea.df<-merge(biomeArea.df, LUT_biome, by="BIOME_ID")
write.csv(biomeArea.df, "Outputs/Biome_area_base.csv", row.names = F)

biomeAnalysis.df<-biomeArea.df
biomeAnalysis.df$tot_2C<-biomeAnalysis.df$Area_2C+biomeAnalysis.df$noAnalog_2C
biomeAnalysis.df$tot_4C<-biomeAnalysis.df$Area_4C+biomeAnalysis.df$noAnalog_4C
biomeAnalysis.df$diff_2C<-biomeAnalysis.df$tot_2C - biomeAnalysis.df$Area_now
biomeAnalysis.df$diff_4C<-biomeAnalysis.df$tot_4C - biomeAnalysis.df$Area_now
biomeAnalysis.df$base_diff_2C<-biomeAnalysis.df$Area_2C - biomeAnalysis.df$Area_now
biomeAnalysis.df$base_diff_4C<-biomeAnalysis.df$Area_4C - biomeAnalysis.df$Area_now
biomeAnalysis.df$noAnalog_now<-0
biomeAnalysis.df$perc_noAnalog2C<-biomeAnalysis.df$noAnalog_2C / biomeAnalysis.df$Area_now
biomeAnalysis.df$perc_noAnalog4C<-biomeAnalysis.df$noAnalog_4C / biomeAnalysis.df$Area_now
write.csv(biomeAnalysis.df, "Outputs/Biome_area_analysis.csv", row.names = F)

#what % of land area is no analog
biomeArea.df$Area_2C[8]/sum(biomeArea.df$Area_2C)

#generate input data for chord diagram
######################################

calc_flowArea<-function(fromRaster, toRaster)
{
  arr<-array()
  for (i in 0:4)
  {
    #for each ecoregion in the fromRaster, determine where the area goes to in the toRaster
    temp_bin<-toRaster
    temp_bin[fromRaster!=i]=NA
    area<-tapply(area(temp_bin), temp_bin[], sum)
    fullarea<-as.array(rep(NA, 849))
    names(fullarea)<-as.character(seq(0, 848, 1))
    fullarea[names(area)]<-area
    if (i==0)
    {
      arr<-fullarea
    }
    else
    {
      arr<-rbind(arr, fullarea)
    }
    print(i)
  }
  return(arr)
}
temp<-calc_flowArea(ecorast_now_mapped, ecorast_2C_mapped)

temp_bin<-ecorast_2C_mapped
temp_bin[ecorast_now_mapped!=2]=NA
area<-tapply(area(temp_bin), temp_bin[], sum)
fullarea<-as.array(rep(NA, 849))
names(fullarea)<-as.character(seq(0, 848, 1))
fullarea[names(area)]<-area
temp<-rbind(area2, area)
temp<-tapply(area(ecorast_now_mapped), ecorast_now_mapped[], sum)

biorast_now_mapped<-ecorast_now_mapped
biorast_now_mapped<-calc(ecorast_now_mapped, fun=ecotobiome)

ecotobiome<-function(x)
{
  df<-subset(LUT_plus, ECO_ID ==x)
  return(as.numeric(df$BIOME_ID))
}



#Generate rasters by substituting % protected for ecoregion ids
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