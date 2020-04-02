area_now_2C_mask<-read.csv("Tables/area_now_2C_mask.csv")
area_now_4C_mask<-read.csv("Tables/area_now_4C_mask.csv")
perc_compare_master<-read.csv("Tables/PA_perc_comparison_master.csv")

protEcosCount<-countEcos(perc_compare_master, c(1:8, 10:13))

countEcos<-function(datasource, index)
{
  counter=0
  for (i in index)
  {
    df<-subset(datasource, BIOME_ID==i)
    if (counter==0)
    {
      out<-data.frame("BIOME_ID"=i, "PA_numEco_now"=sum(df$PA_area_current>0), "Tot_numEco_now"=sum(df$tot_area_current>0),
                      "PA_numEco_2C"=sum(df$PA_area_2C>0), "Tot_numEco_2C"=sum(df$tot_area_2C>0), "PA_numEco_4C"=sum(df$PA_area_4C>0),"Tot_numEco_4C"=sum(df$tot_area_4C>0))
    }
    else
    {
      out<-rbind(out, c("BIOME_ID"=i, "PA_numEco_now"=sum(df$PA_area_current>0), "Tot_numEco_now"=sum(df$tot_area_current>0),
                         "PA_numEco_2C"=sum(df$PA_area_2C>0), "Tot_numEco_2C"=sum(df$tot_area_2C>0), "PA_numEco_4C"=sum(df$PA_area_4C>0),"Tot_numEco_4C"=sum(df$tot_area_4C>0)))
    }
    counter=counter + 1

    
  }
  out<-rbind(out, c("BIOME_ID"=NA, "PA_numEco_now"=sum(as.numeric(out$PA_numEco_now)), "Tot_numEco_now"=sum(as.numeric(out$Tot_numEco_now)),
                    "PA_numEco_2C"=sum(as.numeric(out$PA_numEco_2C)), "Tot_numEco_2C"=sum(as.numeric(out$Tot_numEco_2C)), "PA_numEco_4C"=sum(as.numeric(out$PA_numEco_4C)),"Tot_numEco_4C"=sum(as.numeric(out$Tot_numEco_4C))))
  out$BIOME_NAME=c(as.character(LUT_biome$BIOME_NAME[index]), "total")
  return(out)
}
write.csv(protEcosCount, "Tables/protEcosCount.csv", row.names=F)
