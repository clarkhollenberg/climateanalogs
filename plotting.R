###Plotting settings for reverse climate analogs ecoregion/protected area analysis


#Violin plots
################################
vio.df<-perc_compare_df[-c(1, 832, 833), ]  #remove the biomes with only one ecoregion (rock and ice, no analog and insuff. data)

pdf("Figures/Violins_area_perc_biome_2C.pdf", width=16,height = 8)

par(mar=c(6,18,1,1),bg=NA)
layout(matrix(1:3, nrow=1, ncol=3), widths= c(4.2, 3, 3)) 
vioplot(sqrt(vio.df$tot_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,
        xlab="sqrt(ecoregion area) (sq-km)")

vioplot(sqrt(vio.df$tot_area_2C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
par(mar=c(6, 1, 1, 1))
vioplot(sqrt(vio.df$PA_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black", rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,
        xlab="sqrt(protected area) (sq-km)", yaxt='n', main="+2C")
axis(1, at=c(0, 100, 200, 300, 400, 500))
vioplot(sqrt(vio.df$PA_area_2C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
vioplot(vio.df$PA_perc_current~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="Proportion of ecoregion area protected", yaxt='n')
axis(1, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
vioplot(vio.df$PA_perc_2C~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
dev.off()

pdf("Figures/Violins_area_perc_biome_4C.pdf", width=16,height = 8)
layout(matrix(1:3, nrow=1, ncol=3), widths= c(4.2, 3, 3)) 
par(mar=c(6,18,1,1),bg=NA)
vioplot(sqrt(vio.df$tot_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="sqrt(ecoregion area) (sq-km)")
vioplot(sqrt(vio.df$tot_area_4C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
par(mar=c(6, 1, 1, 1))
vioplot(sqrt(vio.df$PA_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black", rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="sqrt(protected area) (sq-km)", yaxt='n')
axis(1, at=c(0, 100, 200, 300, 400, 500))
vioplot(sqrt(vio.df$PA_area_4C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
vioplot(vio.df$PA_perc_current~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="Proportion of ecoregion area protected", yaxt='n')
axis(1, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
vioplot(vio.df$PA_perc_4C~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
dev.off()
#########################
pdf("Figures/Violins_area_perc_biome.pdf", width=16,height = 12)
par(mar=c(6,18,2,1),bg=NA)
layout(matrix(1:6, nrow=2, ncol=3, byrow=T), widths= c(4.2, 3, 3)) 
vioplot(sqrt(vio.df$tot_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,
        xlab="sqrt(ecoregion area) (sq-km)")
vioplot(sqrt(vio.df$tot_area_2C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)

par(mar=c(6, 0, 2, 1))
vioplot(sqrt(vio.df$PA_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black", rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,
        xlab="sqrt(protected area) (sq-km)", yaxt='n', main="+2C")
axis(1, at=c(0, 100, 200, 300, 400, 500))
vioplot(sqrt(vio.df$PA_area_2C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)

vioplot(vio.df$PA_perc_current~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="Proportion of ecoregion area protected", yaxt='n')
axis(1, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
vioplot(vio.df$PA_perc_2C~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)

par(mar=c(6,18,2,1),bg=NA)
vioplot(sqrt(vio.df$tot_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="sqrt(ecoregion area) (sq-km)")
vioplot(sqrt(vio.df$tot_area_4C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
par(mar=c(6, 0, 2, 1))
vioplot(sqrt(vio.df$PA_area_current)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black", rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,
        xlab="sqrt(protected area) (sq-km)", yaxt='n', main='+4C')
axis(1, at=c(0, 100, 200, 300, 400, 500))
vioplot(sqrt(vio.df$PA_area_4C)~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
vioplot(vio.df$PA_perc_current~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,
        col="NA",border="black",rectCol="NA",lineCol="black",colMed="black",side="right",plotCentre="line",wex=1,areaEqual=F,xlab="Proportion of ecoregion area protected", yaxt='n')
axis(1, at=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))
vioplot(vio.df$PA_perc_4C~vio.df$BIOME_NAME, horizontal=T,las=2,ylab="",cex.axis=0.75,add=T, col="NA",
        border="orange",rectCol="NA",lineCol="orange",colMed="brown",side="left",plotCentre="line",areaEqual=F,wex=1)
dev.off()