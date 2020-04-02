#to assess the effect of modeling heterogeneity  - run on Cortana
#calculates the number of ecoregions in each PA for each scenario
#######################################################################################
PA_rast<-raster("Rasters/PA_noass_rast.tif")
t<-unique(PA_rast)[1]
calc_ecosPerPA<-function(ecorast, filename)
{
  for (i in (unique(PA_rast)))
  {
    print(i)
    tempy<-PA_rast
    tempy[tempy[]!=i]<-NA
    temp<-mask(ecorast, tempy)
    if (i != t)
    {
      df<-rbind(df, c(i, unique(temp), length(unique(temp))))
    }
    else
    {
      df<-data.frame("PA_id"=i, "ecos"=unique(temp), "num_ecos"=length(unique(temp)))
    }
  }
  write.csv(df, paste0("Tables/", filename), row.names=F)
}

calc_ecosPerPA(ecorast_now_mapped, "ecos_per_PA_now.csv")
calc_ecosPerPA(ecorast_2C_mapped, "ecos_per_PA_2C.csv")
calc_ecosPerPA(ecorast_4C_mapped, "ecos_per_PA_4C.csv")