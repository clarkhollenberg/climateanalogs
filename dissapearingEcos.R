PA_perc_comp<-read.csv("Tables/PA_perc_comparison_master.csv")

PA_perc_comp<-subset(PA_perc_comp, tot_area_current!=0)

#count dissapearing ecoregions
#2C -47/810 ~6%
sum(PA_perc_comp$tot_area_2C==0)
#4C  -93/810  ~11%
sum(PA_perc_comp$tot_area_4C==0)

#per biome
numEcos<-aggregate(.~BIOME_ID, FUN=function(x){sum(x!=0)}, 
          data=PA_perc_comp[c("BIOME_ID", "tot_area_current", "tot_area_2C", "tot_area_4C")])
numEcos$prop2C<-numEcos$tot_area_2C/numEcos$tot_area_current
numEcos$prop4C<-numEcos$tot_area_4C/numEcos$tot_area_current

numEcos$propTotalLoss2C<-(numEcos$tot_area_current-numEcos$tot_area_2C)/47
numEcos$propTotalLoss4C<-(numEcos$tot_area_current-numEcos$tot_area_4C)/93
numEcos$nLost2C<-(numEcos$tot_area_current-numEcos$tot_area_2C)
numEcos$nLost4C<-(numEcos$tot_area_current-numEcos$tot_area_4C)

temp<-aggregate(.~BIOME_ID, FUN=mean, 
                       data=PA_perc_comp[c("BIOME_ID", "tot_area_current")])
numEcos$totAveArea<-temp$tot_area_current
temp<-subset(PA_perc_comp, tot_area_2C==0)
temp2<-aggregate(.~BIOME_ID, FUN=mean, 
                 data=temp[c("BIOME_ID", "tot_area_current")])
colnames(temp2)<-c("BIOME_ID", "lostAveArea_2C")
numEcos<-merge(numEcos, temp2, by="BIOME_ID", all=T)
temp<-subset(PA_perc_comp, tot_area_4C==0)
temp2<-aggregate(.~BIOME_ID, FUN=mean, 
                 data=temp[c("BIOME_ID", "tot_area_current")])
colnames(temp2)<-c("BIOME_ID", "lostAveArea_4C")
numEcos<-merge(numEcos, temp2, by="BIOME_ID", all=T)
numEcos<-numEcos[c(1:8, 10:13),]
numEcos$BIOME_NAME<-LUT_biome$BIOME_NAME[c(1:8, 10:13)]

write.csv(numEcos, "Tables/lostEcos.csv", row.names = F)
