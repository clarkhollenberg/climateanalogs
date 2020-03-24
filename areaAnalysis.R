#analyze percent protected changes
##############################################
setwd("~/Insync/clark.hollenberg@gmail.com/Google Drive/Analogs")
load(".RData")
# PA<-raster("~/ClimateAnalogs/analysis/PA/PA.IVInoass.r.GTE75perc_2019-12-23.tif") #laptop
# PA<-raster("InRasters/PA_rast.tif") 
# # convert to binary image
# PA_bin <- PA
# PA_bin[PA>0]<-1 
# rm(PA)

LUT_plus<-read.csv("Tables/LUT_plus.csv")

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

analyzePAsubset<-function(perc_compare_df)
{
  for (i in 1:nrows(perc_compare_df))
  {
    tot_area_sub<-min(perc_compare_df[i, ]$tot_area_current, perc_compare_df[i, ]$tot_area_2C, perc_compare_df[i, ]$tot_area_4C)
    
  }
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

#created masked No Analogs comparisons for MPG investigation
######################################################################################
ecorast_now_masked_2C<-mask(ecorast_now_mapped, ecorast_2C_mapped, maskvalue=847)
ecorast_now_masked_4C<-mask(ecorast_now_mapped, ecorast_4C_mapped, maskvalue=847)
area_now_2C_mask.df<-analyzeByPerc(ecorast_now_masked_2C, 'current')
area_now_4C_mask.df<-analyzeByPerc(ecorast_now_masked_4C, 'current')
write.csv(area_now_2C_mask.df, "Tables/area_now_2C_mask.csv", row.names=F)
write.csv(area_now_4C_mask.df, "Tables/area_now_4C_mask.csv", row.names=F)

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
# LUT_biome<-data.frame("BIOME_ID"=LUT_plus$BIOME_ID, "BIOME_NAME"=LUT_plus$BIOME_NAME, "BIOME_COLOR"=LUT_plus$biome_color)
# LUT_biome<-unique(LUT_biome)
#order LUT first to make sure the biomes map
biomeArea.df<-merge(LUT_biome, biomeArea.df,  by="BIOME_ID")
biomeArea.df$BIOME_COLOR<-NULL
# write.csv(biomeArea.df, "Outputs/Biome_area_barplot.csv", row.names = F)

#include protected area
biomeArea.df$PA_now<-tapply(perc_compare_df$PA_area_current,perc_compare_df$BIOME_ID,FUN = sum)
biomeArea.df$PA_2C<-tapply(perc_compare_df$PA_area_2C,perc_compare_df$BIOME_ID,FUN = sum)
biomeArea.df$PA_4C<-tapply(perc_compare_df$PA_area_4C,perc_compare_df$BIOME_ID,FUN = sum)
write.csv(biomeArea.df, "Outputs/Biome_area_base.csv", row.names = F)

#look at how the total areas change if you include no analogs
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




#generate input data for chord diagram
######################################

#run this on Cortana for ecoregion rasters
###!!!make sure to adjust length if you are running by biome or ecoregion (both in the for loop and the conditional)
calc_flowArea<-function(fromRaster, toRaster, biome=F)
{
  if (biome)
  {
    for (i in 1:16) #ignore insuff. data (biome==17)
    {
      #for each biome in the fromRaster, determine where the area goes to in the toRaster
      temp_bin<-toRaster
      temp_bin[fromRaster!=i]=NA
      #sum areas in toRaster (+2C/+4C) that originated from the focal biome (row index)
      area<-tapply(raster::area(temp_bin), temp_bin[], sum)
      #generate array of NAs with BIOME_ID names
      fullarea<-as.array(rep(NA, 16))
      names(fullarea)<-as.character(seq(1, 16, 1))
      #insert biome area totals at correct locations in array (tapply returns named vector)
      fullarea[names(area)]<-area
      #either create new array or rbind this row
      if (i==1)
      {arr<-fullarea}
      else
      {arr<-rbind(arr, fullarea)}
      print(i)
    }
  }
  #for ecoregions (BIOME==F)
  else
  {
    for (i in 0:847) #ignore insuff. data (ecoid==848)
    {
      #for each ecoregion in the fromRaster, determine where the area goes to in the toRaster
      temp_bin<-toRaster
      temp_bin[fromRaster!=i]=NA
      #sum areas in toRaster (+2C/+4C) that originated from the focal ecoregion (row index)
      area<-tapply(raster::area(temp_bin), temp_bin[], sum)
      fullarea<-as.array(rep(NA, 848)) #848 to include 0 index
      names(fullarea)<-as.character(seq(0, 847, 1))
      fullarea[names(area)]<-area
      if (i==0)
      {arr<-fullarea}
      else
      {arr<-rbind(arr, fullarea)}
      print(i)
    }
  }
  return(arr)
}

#divide by rowSums to transform to probability matrix

#calculate % of each ecoregion that transitioned to a new biome
###################################################################
now_2C_flowMatrix<-read.csv("TransitionMat/now-2C_flow_matrix.csv" ) %>% cleanInput(., row=T) %>% transitionMatrix() 
intrabiome_2C.df<-calc_biomeShift(now_2C_flowMatrix) %>% merge(., LUT_plus, by="econame")
now_4C_flowMatrix<-read.csv("TransitionMat/now-4C_flow_matrix.csv") %>% cleanInput(., row=T) %>% transitionMatrix() 
intrabiome_4C.df<-calc_biomeShift(now_4C_flowMatrix) %>% merge(., LUT_plus, by="econame")
write.csv(intrabiome_2C.df, "Tables/intrabiome_trans_2C_by_eco.csv", row.names=F)
write.csv(intrabiome_4C.df, "Tables/intrabiome_trans_4C_by_eco.csv", row.names=F)
intrabiome_2C.df<-read.csv("Tables/intrabiome_trans_2C_by_eco.csv")
intrabiome_2C.df<-intrabiome_2C.df[c(2,3)]
intrabiome_4C.df<-read.csv("Tables/intrabiome_trans_4C_by_eco.csv")
intrabiome_4C.df<-intrabiome_4C.df[c(2,3)]
intrabiome_2C_rast<-subs(ecorast_now_mapped, intrabiome_2C.df, by="ECO_ID", which="perc_in_biome")
intrabiome_4C_rast<-subs(ecorast_now_mapped, intrabiome_4C.df, by="ECO_ID", which="perc_in_biome")
plot(intrabiome_4C_rast, col=colorRampPalette(c("red", "light gray", "blue"))(50))
intrabiome_2C.df$perc_biome_trans<-1-intrabiome_2C.df$perc_in_biome
intrabiome_4C.df$perc_biome_trans<-1-intrabiome_4C.df$perc_in_biome
#summarize by bioime
intrabiome_shift_biome2C.df<-ave_ColByBiome(intrabiome_2C.df, "perc_biome_trans", c(1:8, 10:13))
intrabiome_shift_biome4C.df<-ave_ColByBiome(intrabiome_4C.df, "perc_biome_trans", c(1:8, 10:13))

interbiome_shift_byBiome.df<-intrabiome_shift_biome2C.df
colnames(interbiome_shift_byBiome.df)[2]<-"Mean_biome_trans_2C"
interbiome_shift_byBiome.df$Mean_biome_trans_4C<-intrabiome_shift_biome4C.df$Mean_biome_transition
interbiome_shift_byBiome.df<-interbiome_shift_byBiome.df[c(1, 3, 2, 5)]

#for each row in transition matrix, calculate percent that changed biomes (by ecoregion)
calc_biomeShift<-function(transMat)
{
  for (i in 1:nrow(transMat))  #we don't need to include the no Analog condition
  {
    df<-transMat[i, ] #pull the row of interest
    biome<-subset(LUT_plus, econame==rownames(df)[1])  #select row from LUT with same econame
    biome<-subset(LUT_plus, BIOME_NAME==biome$BIOME_NAME[1]) #select rows with same biome name as ecoregion
    temp<-df[, colnames(transMat) %in% biome$econame]
    print(i)
    if (i==1)
    {out<-data.frame("perc_in_biome"=sum(temp))}
    else
    {
      out<-rbind(out, sum(temp))
    }
  }
  out$econame<-rownames(transMat) #create column with matching econames
  return(out)
}


#generate biomeid based rasters
ecotobiome<-function(x)
{
  return(LUT_plus$BIOME_ID[x+1])
}
biorast_now_mapped<-calc(ecorast_now_mapped, fun=ecotobiome)
biorast_2C_mapped<-calc(ecorast_2C_mapped, fun=ecotobiome)
biorast_4C_mapped<-calc(ecorast_4C_mapped, fun=ecotobiome)

#calculate matrix showing biome area fluxes
biome_2C_flow_matrix<-calc_flowArea(biorast_now_mapped, biorast_2C_mapped, biome=T)
biome_4C_flow_matrix<-calc_flowArea(biorast_now_mapped, biorast_4C_mapped, biome=T)
rownames(biome_2C_flow_matrix)<-colnames(biome_2C_flow_matrix)
rownames(biome_4C_flow_matrix)<-colnames(biome_4C_flow_matrix)
biome_2C_flow_matrix[is.na(biome_2C_flow_matrix)]<-0
biome_4C_flow_matrix[is.na(biome_4C_flow_matrix)]<-0
write.csv(biome_2C_flow_matrix, "TransitionMat/biome_2C_flow_matrix.csv", row.names=F)
write.csv(biome_4C_flow_matrix, "TransitionMat/biome_4C_flow_matrix.csv", row.names=F)

#flows within/between protected areas
PA_eco_now<-mask(ecorast_now_mapped, PA_bin)
PA_eco_2C<-mask(ecorast_2C_mapped, PA_bin)
PA_eco_4C<-mask(ecorast_4C_mapped, PA_bin)
PA_eco_2C_flowMatrix<-calc_flowArea(PA_eco_now, PA_eco_2C)
PA_eco_4C_flowMatrix<-calc_flowArea(PA_eco_now, PA_eco_4C)

#local area flows - try western us
local_flowCalc<-function(fromRaster, toRaster, extentVector)
{
  bounds<-extent(extentVector)
  fromRaster<-crop(fromRaster, bounds)
  toRaster<-crop(toRaster, bounds)
  return(calc_flowArea(fromRaster, toRaster))
}

westUSA_2Cflow_matrix<-local_flowCalc(ecorast_now_mapped, ecorast_2C_mapped, c(-125, -105, 32, 49))
westUSA_4Cflow_matrix<-local_flowCalc(ecorast_now_mapped, ecorast_4C_mapped, c(-125, -105, 32, 49))
write.csv(westUSA_2Cflow_matrix, "Outputs/westUSA_ecorgn2C_flux.csv", row.names=F)
write.csv(westUSA_4Cflow_matrix, "Outputs/westUSA_ecorgn4C_flux.csv", row.names=F)



#Generate rasters by substituting % protected for ecoregion ids
perc_current_rast<-subs(ecorgn_rast_now, perc_current_df, by="ECO_ID",which="PA_perc_current")
perc_2C_rast<-subs(ecorgn_rast_2C, perc_2C_df, by="ECO_ID",which="PA_perc_2C")
perc_4C_rast<-subs(ecorgn_rast_4C, perc_4C_df, by="ECO_ID",which="PA_perc_4C")
perc_change_rast_2C<-perc_2C_rast - perc_current_rast
perc_change_rast_4C<-perc_4C_rast - perc_current_rast

#function to take any variable column for ecoregions and average by biomes
#returns dataframe
ave_ColByBiome<-function(datasource, colname, index, suffix)
{
  counter=0
  for (i in index)
  {
    df<-subset(datasource, BIOME_ID==i)
    temp<-sum(as.numeric(df[,colname]))/length(df[,colname])  #average by biome
    if (counter==0)
    {
      out<-data.frame("BIOME_ID"=i, "Mean_biome_transition"=temp)
    }
    else
    {
      out<-rbind(out, c("BIOME_ID"=i, "Mean_biome_transition"=temp))
    }
    counter=counter + 1
  }
  colnames(out)<-c("BIOME_ID", paste0("mean_biome_perc_PA_", suffix))
  return(out)
}
PA_perc_comp_master<-read.csv("Tables/PA_perc_comparison_master.csv")
PA_bio_perc_comp_master<-data.frame("BIOME_ID"=1:16, "BIOME_NAME"=LUT_biome$BIOME_NAME[1:16], "perc_PA_now"=ave_percProtByBiome(PA_perc_comp_master, "tot_area_current", "PA_area_current"),
                                    "perc_PA_2C"=ave_percProtByBiome(PA_perc_comp_master, "tot_area_2C", "PA_area_2C"),
                                    "perc_PA_4C"=ave_percProtByBiome(PA_perc_comp_master, "tot_area_4C", "PA_area_4C"))
#this contains total % protected by biome (weighted ecoregion area)
write.csv(PA_bio_perc_comp_master, "Tables/PA_perc_by_biome.csv", row.names=F)

PA_bio_ave_perc_comp<-ave_ColByBiome(PA_perc_comp_master, "PA_perc_current", c(1:16), "now")
temp<-ave_ColByBiome(PA_perc_comp_master, "PA_perc_2C", c(1:16), "2C")
temp2<-ave_ColByBiome(PA_perc_comp_master, "PA_perc_4C", c(1:16), "4C")
PA_bio_ave_perc_comp<-merge(PA_bio_ave_perc_comp, temp, by="BIOME_ID")
PA_bio_ave_perc_comp<-merge(PA_bio_ave_perc_comp, temp2, by="BIOME_ID")
PA_bio_ave_perc_comp$BIOME_NAME<-LUT_biome$BIOME_NAME[1:16]
PA_bio_ave_perc_comp<-PA_bio_ave_perc_comp[c(1, 5, 2, 3, 4)]
#this averages the ecoregion % protected by biome, so all ecoregions are weighted equally
write.csv(PA_bio_ave_perc_comp, "Tables/PA_meanperc_by_biome.csv", row.names=F)

ave_percProtByBiome<-function(datasource, colnametot, colnamePA=NULL, PA=F)
{
  for (i in 1:16)
  {
    
    df<-subset(datasource, BIOME_ID==i)
    temp<-sum(as.numeric(df[,colnametot]))
    temp2<-sum(as.numeric(df[,colnamePA]))
    #double check that this is the correct column name in the datasource before running this
    if (i==1)
    {
      vec<-temp2/temp
    }
    else
    {vec<-c(vec, temp2/temp)}
    
  }
  return(vec)
}