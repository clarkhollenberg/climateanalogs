### metrics for evaluating representation for conservation targets
## Date: 3/4/20

data<-read.csv("Tables/PA_perc_comparison_master.csv",header=T)

######
# percent protected graphs by biome (distribution)
##############################
pdf("Graphs/Perc_prot_shifts_bio.pdf", 20, 10)
par(mfrow=c(1,2))
for (i in c(1:8, 10:13)){
  temp<-data[data$BIOME_ID==i, ]
  sub<-subset(temp, tot_area_current!=0)
  sub<-sub[order(sub$tot_area_current),]
  x<-1:length(sub$ECO_ID)
  x2Cp<-x[sub$tot_area_2C==0]
  x4Cp<-x[sub$tot_area_4C==0]
  plot(x, sub$PA_perc_current, col="dark green", type="l", main=paste(as.character(unique(temp$BIOME_NAME)), "ordered by tot_area_now"), ylab="PA_perc", ylim=c(0, 1))
  lines(x, sub$PA_perc_2C, col="orange")
  if (length(x2Cp)!=0)
  {points(x2Cp, rep(0, length(x2Cp)), col="dark orange", pch=15)}
  lines(x, sub$PA_perc_4C, col="red")
  if (length(x4Cp)!=0)
  {points(x4Cp, rep(0, length(x4Cp)), col="dark red", pch=20)}
  abline(h=0.3, lty="dashed")
  legend("topright", legend=c("now", "+2C", "+4C"), fill=c("dark green", "orange", "red"))
  
  temp<-data[data$BIOME_ID==i, ]
  sub<-subset(temp, tot_area_current!=0)
  sub<-sub[order(sub$PA_perc_current),]
  sub2C<-sub[order(sub$PA_perc_2C),]
  sub4C<-sub[order(sub$PA_perc_4C),]
  x<-1:length(sub$ECO_ID)
  x2Cp<-x[sub$tot_area_2C==0]
  x4Cp<-x[sub$tot_area_4C==0]
  plot(x, sub$PA_perc_current, col="dark green", type="l", main=paste(as.character(unique(temp$BIOME_NAME)), "ordered by PA_perc"), ylab="PA_perc", ylim=c(0, 1))
  lines(x[(sum(sub2C$tot_area_2C==0)+1):length(x)], sub2C[sub2C$tot_area_2C!=0, ]$PA_perc_2C, col="orange")
  if (length(x2Cp)!=0)
  {points(x2Cp, sub[x2Cp, ]$PA_perc_current, col="dark orange", pch=15)}
  lines(x[(sum(sub2C$tot_area_4C==0)+1):length(x)], sub4C[sub4C$tot_area_4C!=0, ]$PA_perc_4C, col="red")
  if (length(x4Cp)!=0)
  {points(x4Cp, sub[x4Cp, ]$PA_perc_current, col="dark red", pch=20)}
  abline(h=0.3, lty="dashed")
  legend("topleft", legend=c("now", "+2C", "+4C"), fill=c("dark green", "orange", "red"))
}
dev.off()

