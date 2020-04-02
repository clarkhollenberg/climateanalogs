intrabiome_2C.df<-read.csv("Tables/intrabiome_trans_2C_by_eco.csv")
intrabiome_2C<-1-intrabiome_2C.df$perc_in_biome
intrabiome_4C.df<-read.csv("Tables/intrabiome_trans_4C_by_eco.csv")
intrabiome_4C<-1-intrabiome_4C.df$perc_in_biome
eco_trans.df<-read.csv("Tables/trans_table_eco.csv")


biome_cumDist_2C<-cumulativeDist(1-intrabiome_2C.df$perc_in_biome)
biome_cumDist_4C<-cumulativeDist(1-intrabiome_4C.df$perc_in_biome)

eco_cumDist_2C<-cumulativeDist(eco_trans.df$perc_trans_2C/100)
eco_cumDist_4C<-cumulativeDist(eco_trans.df$perc_trans_4C/100)

cumulativeDist<-function(data)
{
  data<-data[!is.na(data)]
  for (i in seq(0, 1, by=0.01))
  {
    
    df<-data[data>i]
    if (i==0)
    {val<-length(df)
    ind<-i}
    else{ 
      val<-append(val, length(df))
      ind<-append(ind, i)
    }
  }   
  return (data.frame( "Percent_biome_shift"=ind, "Num_ecorgns"=val))
}

pdf("Graphs/dist_of_biome_shifts.pdf", 12, 8)
par(mfrow=c(1, 2))
plot(biome_cumDist_2C, xlab="Proportion of ecoregions", ylab= "% ecoregion area changing biome",
     main="+2C CDF biome change")
plot(biome_cumDist_4C, xlab="Proportion of ecoregions", 
     ylab= "% ecoregion area changing biome",main="+4C CDF biome change")
dev.off()

pdf("Graphs/dist_of_eco_shifts.pdf", 12, 8)
par(mfrow=c(1, 2))
plot(eco_cumDist_2C, xlab="Proportion of ecoregion shift", ylab= "Number of ecoregions",
     main="+2C distribution %ecoregion area shift")
plot(eco_cumDist_4C, xlab="Proportion of ecoregion shift", 
     ylab= "Number of ecoregions",main="+4C distribution %ecoregion area shift")
dev.off()
