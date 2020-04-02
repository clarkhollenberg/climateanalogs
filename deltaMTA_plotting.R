#delta MTA vs ecorgn size
mta_eco_delta<-read.csv("Tables/mta_by_eco_delta.csv")
PA_perc_master<-read.csv("Tables/PA_perc_comparison_master.csv")
mta_delta_size<-merge(mta_eco_delta, PA_perc_master, by="ECO_ID")
lmarea<-lm(abs(deltaMTA_2C) ~ tot_area_current, data=mta_delta_size)
lmarea4C<-lm(abs(deltaMTA_4C) ~ tot_area_current, data=mta_delta_size)
pdf("Graphs/deltaMTAvsEcoSize.pdf", 16, 8)
par(mfrow=c(1,2))
plot(mta_delta_size$tot_area_current, abs(mta_delta_size$deltaMTA_2C), xlim=c(0, 1e+06))
abline(1.84e-01, -1.137e-07, lty="dashed")
plot(mta_delta_size$tot_area_current, abs(mta_delta_size$deltaMTA_4C), xlim=c(0, 1e+06))
abline(2.67e-01, -1.050e-07, lty="dashed")
dev.off()
plot(lm)

pdf("Graphs/hist_eco_size.pdf", 16, 8)
par(mfrow=c(1,3))
hist(mta_delta_size$tot_area_current, breaks=100)
hist(mta_delta_size$tot_area_current, breaks=200, xlim=c(0,1e+06))
hist(mta_delta_size$tot_area_current, breaks=1000, xlim=c(0,5e+04))
dev.off()
pdf("Graphs/hist_eco_mta.pdf", 16, 8)
par(mfrow=c(1,2))
hist(mta_delta_size$deltaMTA_2C, breaks=100)
hist(mta_delta_size$deltaMTA_4C, breaks=100)
dev.off()

#plotting delta MTA boxplot
##############################################
mta_by_eco.df<-read.csv("Tables/mta_by_eco_plus.csv")
melted<-mta_by_eco.df
melted<-melted[melted$BIOME_ID %notin% c(9, 14, 15, 16, 17), ]#remove rock and ice, no analogs, insufficient data and water based biomes
melted$deltaMTA_2C<-melted$mta_2C_incl0-melted$mta_now
melted$deltaMTA_4C<-melted$mta_4C_incl0-melted$mta_now
melted<-melted[c(11, 14, 15)]

melted<-melt(melted, c("BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
pdf("Figures/mta_delta_box_plot_biome.pdf", 16, 8)
ggplot(melted, aes(x=variable, y=value, fill=variable)) + coord_flip()  + 
  geom_boxplot() + scale_fill_manual(values=c("orange", "red")) + geom_hline(yintercept = 0, linetype="dashed") +
  facet_grid(BIOME_NAME ~ .) + theme(strip.text.y = element_text(angle = 360))
dev.off()

#delta MTA with error bars
###########################################
library(reshape2)
source("C:/Users/clark/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/bootstrapCI.R")
mta_eco_delta<-read.csv("Tables/mta_by_eco_delta.csv")
melted<-mta_eco_delta[c(6,8,9)]
melted<-aggregate(.~BIOME_ID, data=melted, mean)  #wow key function!!!
melted$BIOME_NAME<-LUT_biome$BIOME_NAME[c(1:8, 10:13)]

melted<-melt(melted, c("BIOME_ID", "BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
melted$scenario<-''
melted[melted$variable=='deltaMTA_2C', ]$scenario<- "+twoC"
melted[melted$variable=='deltaMTA_4C', ]$scenario<- "+fourC"
errors<-rbind(mta_biomeBootstrap(mta_eco_delta, mta_eco_delta$deltaMTA_2C), 
              mta_biomeBootstrap(mta_eco_delta, mta_eco_delta$deltaMTA_4C))
colnames(errors)=c("lower", "upper")
melted<-cbind(melted, errors)
pdf("Figures/Graphs/delta_mta_biome_bar_plot.pdf", 16, 8)
ggplot(melted, aes(x=scenario, y=value, fill=variable)) +
  geom_bar(stat='identity', position='stack') + geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9)) +
  geom_hline(yintercept = 0, linetype="dashed")+coord_flip()  + 
  scale_fill_manual(values=c("orange", "red")) + facet_grid(BIOME_NAME ~ .) + 
  theme(strip.text.y = element_text(angle = 360))
dev.off()


#proportion of extreme deltaMTA values per biome
#################################################
mta_eco_delta<-read.csv("Tables/mta_by_eco_delta.csv")
mta_eco_delta<-mta_eco_delta[c(6, 8, 9)]
abs02<-function(x){x<-x[!is.na(x)];sum(abs(x)>0.2)/length(x)}
t<-aggregate(.~BIOME_ID, FUN=abs02, data=mta_eco_delta)
t2<-aggregate(.~BIOME_ID, FUN=bootstrapCIbase, data=mta_eco_delta)
colnames(t2)=c("BIOME_ID", "se_2C", "se_4C")
colnames(t)=c("BIOME_ID", "+2C", "+4C")
melted<-t
melted$BIOME_NAME<-LUT_biome$BIOME_NAME[c(1:8, 10:13)]

melted<-melt(melted, c("BIOME_ID", "BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
melted$SE<-c(t2$se_2C, t2$se_4C)

pdf("Figures/Graphs/deltaMTA_over_abs20p_biome_bar_plot.pdf", 16, 8)
ggplot(melted, aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat='identity', position='stack') + #geom_errorbar(aes(ymin=value-SE, ymax=value+SE), position=position_dodge(.9)) +
  geom_hline(aes(yintercept= 0.27, linetype = "+2C")) +
  geom_hline(aes(yintercept= 0.42, linetype = "+4C")) +
  coord_flip()  + 
  scale_fill_manual(name="Prop. of ecoregions abs(deltaMTA)>0.2", values=c("orange", "red")) + 
  scale_linetype_manual(name = "Global proportion", values = c("dashed", "F1"))+
  facet_grid(BIOME_NAME ~ .) + 
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 360))+
  ggtitle("Proportion of extreme MTA change by biome (abs>20%)")
dev.off()
######################################
#greater than 20% loss in MTA
t<-aggregate(.~BIOME_ID, FUN=function(x){x<-x[!is.na(x)];sum(x<(-0.2))/length(x)}, data=mta_eco_delta)
t2<-aggregate(.~BIOME_ID, FUN=bootstrapCIbase, data=mta_eco_delta)
colnames(t2)=c("BIOME_ID", "se_2C", "se_4C")
colnames(t)=c("BIOME_ID", "+2C", "+4C")
melted<-t
melted$BIOME_NAME<-LUT_biome$BIOME_NAME[c(1:8, 10:13)]

melted<-melt(melted, c("BIOME_ID", "BIOME_NAME"))  #melt dataframe w/ Biome_id and name as id columns
melted$SE<-c(t2$se_2C, t2$se_4C)


pdf("Figures/Graphs/deltaMTA_under_20p_biome_bar_plot.pdf", 16, 8)
ggplot(melted, aes(x=variable, y=value, fill=variable)) +
  geom_bar(stat='identity', position='stack') +
  geom_hline(aes(yintercept= 0.12, linetype = "+2C")) +
  geom_hline(aes(yintercept= 0.19, linetype = "+4C")) +
  coord_flip()  + 
  scale_fill_manual(name="Prop. of ecoregions deltaMTA<-0.2", values=c("orange", "red")) + 
  scale_linetype_manual(name = "Global proportion", values = c("dashed", "F1"))+
  facet_grid(BIOME_NAME ~ .) + 
  theme(axis.title.y=element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.y = element_text(angle = 360))+
  ggtitle("Proportion of extreme MTA loss by biome (<-20%)")
dev.off()



#comparison in perc protected
pdf(file="Figures/Graphs/Perc_protected_comparison_graph_biome.pdf", 20, 8)
par(mfrow=c(1,3))
for (i in 1:14)
{
  vec <- vector()
  df<-subset(perc_compare_df, BIOME_ID==i)
  # for (i in 1:nrow(df))
  # {
  #   temp<-df$Mean_vote_perc[i]*100
  #   temp<-as.integer(temp)
  #   #choose the color for the ecoregion based on the value of the average % vote
  #   vec <- c(vec, colorRampPalette(c('red', 'gray', 'blue'))(100)[temp])
  # }
  plot(df$PA_perc_current, df$PA_perc_4C, xlim= c(0, 1), ylim= c(0, 1), xlab = 'Current proportion protected', 
       ylab= '+4C proportion protected', main=unique(df$BIOME_NAME))
  # legend("topleft", title = "% vote mean", legend = seq(from=1, to=0, by = -0.1), fill=colorRampPalette(c('blue', 'gray', 'red'))(11))
  abline(0, 1, lty = 'dashed')
}
dev.off()