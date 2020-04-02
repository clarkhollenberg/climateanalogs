####graph plotting functions for ecoregion reverse climate analogs

`%notin%` <- Negate(`%in%`)

#biome area changes barplot
###########################################
library(ggplot2)
library(reshape2)
melted<-biomeArea.df
melted<-melted[melted$BIOME_ID %notin% c(9, 14, 15, 16, 17), ]  #remove rock and ice, no analogs, insufficient data and water based biomes
# temp<-sapply(melted[, 2:6], log10)
melted<-melt(melted, c("BIOME_ID", "BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
melted$area<-melted$value

melted$scenario<-''
melted[melted$variable=='Area_now', ]$scenario<- "Now"
melted[melted$variable=='Area_2C' | melted$variable=='noAnalog_2C', ]$scenario<- "+twoC"
melted[melted$variable=='Area_4C' | melted$variable=='noAnalog_4C', ]$scenario<- "+fourC"
melted$variable <- relevel(melted$variable, "noAnalog_4C")
melted$variable <- relevel(melted$variable, "noAnalog_2C")

pdf("Figures/Biome_area_bar_plot.pdf", 16, 8)
ggplot(melted, aes(x=scenario, y=area, fill=variable)) +
  geom_bar(stat='identity', position='stack') + coord_flip()  + 
  scale_fill_manual(values=c("black", "black", "dark green", "orange", "red")) + facet_grid(BIOME_NAME ~ .) + 
  theme(strip.text.y = element_text(angle = 360))
dev.off()
#####################################

#MTA by biome with 95% CI bars
################################################
mta_biome<-read.csv("Tables/biomeMTA_factorCorr.csv")
melted<-mta_biome[c(1,2,5,8)]
melted<-melt(melted, c("BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
melted$scenario<-''
melted[melted$variable=='mta_now', ]$scenario<- "Now"
melted[melted$variable=='mta_2C_incl0', ]$scenario<- "+twoC"
melted[melted$variable=='mta_4C_incl0', ]$scenario<- "+fourC"
errors<-rbind(mta_biomeBootstrap(mta_by_eco.df, mta_by_eco.df$mta_now), mta_biomeBootstrap(mta_by_eco.df, mta_by_eco.df$mta_2C_incl0),
              mta_biomeBootstrap(mta_by_eco.df, mta_by_eco.df$mta_4C_incl0))
colnames(errors)=c("lower", "upper")
melted<-cbind(melted, errors)
pdf("Figures/mta_biome_bar_plot.pdf", 16, 8)
ggplot(melted, aes(x=scenario, y=value, fill=variable)) +
  geom_bar(stat='identity', position='stack') + geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9)) +coord_flip()  + 
  scale_fill_manual(values=c("dark green", "orange", "red")) + facet_grid(BIOME_NAME ~ .) + 
  theme(strip.text.y = element_text(angle = 360))
dev.off()

#box and whisker MTA by biome
##########################
library(ggplot2)
library(reshape2)
mta_by_eco.df<-read.csv("Tables/mta_by_eco_plus.csv")
melted<-mpg_by_eco.df
melted<-melted[melted$BIOME_ID %notin% c(9, 14, 15, 16, 17), ]#remove rock and ice, no analogs, insufficient data and water based biomes
melted<-melted[c(2,7,8,11)]
melted<-melt(melted, c("BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns

melted$scenario<-''
melted[melted$variable=='mta_now', ]$scenario<- "Now"
melted[melted$variable=='mta_2C_incl0', ]$scenario<- "+twoC"
melted[melted$variable=='mta_4C_incl0', ]$scenario<- "+fourC"


pdf("Figures/MTA_box_plot_biome.pdf", 16, 8)
ggplot(melted, aes(x=scenario, y=value, fill=variable)) + coord_flip()  + 
  geom_boxplot() + scale_fill_manual(values=c("dark green", "orange", "red")) +
  facet_grid(BIOME_NAME ~ .) + theme(strip.text.y = element_text(angle = 360))
dev.off()
