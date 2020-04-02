library(raster)
ecorast_now<-raster("Rasters/ecorast_now_mapped.tif")
PA_bin<-raster("Rasters/PA_bin.tif")
area_now<-area(ecorast_now)
area_now<-mask(area_now, ecorast_now)
area_now<-mask(area_now, ecorast_now, maskvalue=848)
globalArea<-rowSums(area_now, na.rm=T)

PA_area<-mask(area_now, PA_bin) 
PA_area<-mask(PA_area, ecorast_now, maskvalue=848)
paArea<-rowSums(PA_area, na.rm=T)


paArea<-colSums(matrix(paArea, ncol=45))
globalArea<-colSums(matrix(globalArea, ncol=45))
prop_PA<-paArea/globalArea

pdf("Graphs/PA_vs_lat.pdf")
#note that the first elements are the coming from the top of the northern hemisphere
plot(seq(89, -89, by=-4), prop_PA, xlab="Latitude", ylab="PA/land area", 
     main="Latitudinal variation in protected area")
dev.off()

t<-data.frame("Lat"=seq(89, -89, by=-4), "prop_PA"=prop_PA)

