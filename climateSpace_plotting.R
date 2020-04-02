now_all.proj<-read.csv("ClimateSpace/Tables/now.proj.csv")
range(now_all.proj$PC1)
range(now_all.proj$PC2)
now_all.proj<-now_all.proj[!now_all.proj$PC1<(-4),]
write.csv(now_all.proj, "ClimateSpace/Tables/now.proj.csv", row.names=F)

assignBins<-function(col, binRange, nBins)
{
  offset<-0-binRange[1]
  scale<-(binRange[2]-binRange[1])/nBins
  temp<-(col+offset)/scale
  temp<-ceiling(temp)
  return(temp)
}

now_all.proj$PC1_bin<-assignBins(now_all.proj$PC1, c(-4, 3.4), 15)
now_all.proj$PC2_bin<-assignBins(now_all.proj$PC2, c(-2.9, 3.6), 15)
now_counted_bins<-aggregate(now_all.proj[c("PC1_bin")], by=list(now_all.proj$PC1_bin, now_all.proj$PC2_bin), FUN=length)
now_counted_bins<-now_counted_bins[order(now_counted_bins$Group.2),]
now_counted_bins<-now_counted_bins[order(now_counted_bins$Group.1),]
colnames(now_counted_bins)<-c("PC1_bin", "PC2_bin", "count")

now_bin_mat<-matrix(rep(0, 15*15), nrow=15, ncol=15)
for (i in 1:nrow(now_counted_bins))
{
  print(i)
  now_bin_mat[now_counted_bins$PC1_bin[i], 15-now_counted_bins$PC2_bin[i]]<-now_counted_bins$count[i]
}
now_bin_rast<-raster(now_bin_mat)
