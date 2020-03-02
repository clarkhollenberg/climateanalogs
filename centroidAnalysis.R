######Calculate centroids for ecoregion##
##################
centroidCalc<-function(ecorgn_rast, filename)
{
  for (i in unique(ecorgn_rast))
  {
    
    xy_i<-colMeans(xyFromCell(ecorgn_rast, which(ecorgn_rast[]==i)))
    if (exists("centr"))
    {
      centr <- rbind(centr,xy_i)
    }
    else
    {
      centr <- xy_i
    }
    print(i)
    
  }
  centr <- cbind(unique(ecorgn_rast), centr)
  cent <- data.frame(centr)
  rownames(centr) <- c()
  colnames(centr) <- c("ECO_ID", "x", "y")
  write.csv(centr, filename)
  return(centr)
}

centroidCalcBiome<-function(ecorgn_rast, filename)
{
  
  for (i in 1:14)
  {
    
    ecobioVector<-subset(sorted_LUT, sorted_LUT$BIOME_ID==i, select=ECO_ID)
    xy_i<-data.frame(xyFromCell(ecorgn_rast, which(ecorgn_rast[] %in% ecobioVector$ECO_ID)))
    xy_N<-subset(xy_i, y>0)
    xy_N<-colMeans(xy_N)
    xy_S<-subset(xy_i, y<0)
    xy_S<-colMeans(xy_S)
    
    if (exists("centr"))
    {
      centr <- rbind(centr, cbind(i, xy_N[1], xy_N[2], xy_S[1], xy_S[2]))
    }
    else
    {
      centr <- cbind(i, xy_N[1], xy_N[2], xy_S[1], xy_S[2])
    }
    print(i)
    colnames(centr)<-c("BIOME_ID", "x_N", "y_N", "x_S", "y_S")
    
  }
  write.csv(centr, filename)
  return(centr)
}

centr_now <- centroidCalc(ecorgn_rast_now, "Current_ecoregion_centroids.csv")
centr_now <- data.frame(centr_now)
rownames(centr_now) <- c()
colnames(centr_now) <- c("ECO_ID", "x_now", "y_now")

# points(cbind(centr_now[,2], centr_now[,3]))
centr_Biome_now<-centroidCalcBiome(ecorgn_rast_now, "biome_centroids_current.csv")
centr_Biome_2C<-centroidCalcBiome(ecorgn_rast_2C, "biome_centroids_2C.csv")
colnames(centr_Biome_now)<-c("ECO_ID", "x_N_now", "y_N_now", "x_S_now", "y_S_now")
colnames(centr_Biome_2C)<-c("ECO_ID", "x_N_2C", "y_N_2C", "x_S_2C", "y_S_2C")
centr_Biome_mastr<-merge(centr_Biome_2C, centr_Biome_now)
centr_Biome_mastr$displacement_km_N <-apply(centr_Biome_mastr, 1, function(k) measure_geodist(k['x_N_now'], k['y_N_now'], k['x_N_2C'], k['y_N_2C']))
centr_Biome_mastr$displacement_km_S <-apply(centr_Biome_mastr, 1, function(k) measure_geodist(k['x_S_now'], k['y_S_now'], k['x_S_2C'], k['y_S_2C']))
centr_Biome_mastr$bearing_N <-apply(centr_Biome_mastr, 1, function(k) measure_bearing(k['x_N_now'], k['y_N_now'], k['x_N_2C'], k['y_N_2C']))
centr_Biome_mastr$bearing_S <-apply(centr_Biome_mastr, 1, function(k) measure_bearing(k['x_S_now'], k['y_S_now'], k['x_S_2C'], k['y_S_2C']))

centr_2C <- centroidCalc(ecorgn_rast_2C, "Centroids_ecoregion_2C.csv")
colnames(centr_2C) <- c("ECO_ID", "x_2C", "y_2C")

centr_mastr <- merge(centr_now, centr_2C)
#remove rock and ice outlier
centr_mastr <- centr_mastr[-c(1),]
#create histogram of latitudinal movements
hist(centr_mastr$displacement_lat, breaks = 100, xlab="North/South Movement (km)", main = "Ecoregion Centroid Latitudinal Shifts")

pdf("Latitudinal Movement by Biome.pdf", paper="letter")
par(mfcol=c(3,1))
for (i in 1:14)
{
  temp<-subset(centr_mastr, BIOME_ID==i)
  hist(temp$displacement_lat_sign, breaks = 20, xlim=c(-1500, 1500), xlab="North/South Movement (km)", main = as.character(unique(temp$BIOME_NAME)))
}
dev.off()

measure_geodist <- function(lon_from, lat_from, lon_to, lat_to)
{
  dist <- round(distm(c(lon_from, lat_from), c(lon_to, lat_to), fun = distHaversine)/1000, digits=2)
  return(dist)
}

measure_latchange <- function(lat_from, lat_to)
{
  dist <- round(distm(c(0, lat_from), c(0, lat_to), fun = distHaversine)/1000, digits=2)
  return(dist)
}

measure_latchange_sign <- function(latChange, bearing)
{
  if ((as.numeric(bearing) < 90) | (as.numeric(bearing) > 270))
  {return(as.numeric(latChange))}
  else
  {return(as.numeric(latChange)*-1)}
}

measure_bearing <- function(lon_from, lat_from, lon_to, lat_to)
{
  if (is.nan(lon_from) | is.nan(lat_from) | is.nan(lon_to) | is.nan(lon_to))
  {
    return(NULL)
  }
  else
  {
    bearing <- round(bearingRhumb(c(lon_from, lat_from), c(lon_to, lat_to)), digits=2)
    return(bearing)
  }
  
}

#Create columns calculating centroid movement
centr_mastr$displacement_km <-apply(centr_mastr, 1, function(k) measure_geodist(k['x_now'], k['y_now'], k['x_2C'], k['y_2C']))
centr_mastr$bearing <-apply(centr_mastr, 1, function(k) measure_bearing(k['x_now'], k['y_now'], k['x_2C'], k['y_2C']))
#Looking at latitude shifts
centr_mastr$displacement_lat <-apply(centr_mastr, 1, function(k) measure_latchange(k['y_now'], k['y_2C']))
centr_mastr$displacement_lat_sign <-apply(centr_mastr, 1, function(k) measure_latchange_sign(k['displacement_lat'], k['bearing']))

##############

##create rose plot showing distance and direction of centroid movement##
#######################
pdf(file= "Rose plots by biome.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=df$BIOME_NAME[1])
  plot(centr_rose)
}
dev.off()

pdf(file= "Rose plots Northern.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  df<-subset(df, y_now>0)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=paste("N", df$BIOME_NAME[1]))
  plot(centr_rose)
}
dev.off()

pdf(file= "Rose plots Southern.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  df<-subset(df, y_now < 0)
  if (nrow(df)!=0)
  {
    centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, n.cyclVar=360)
    plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=paste("S", df$BIOME_NAME[1]))
    plot(centr_rose)
  }
}
dev.off()

pdf(file= "Rose plots by biome aggr.pdf", paper="letter")
for (i in 1:14)
{
  df<-subset(centr_mastr, BIOME_ID==i)
  centr_rose <- rose(df$displacement_km, cyclVar = df$bearing, circle = 360, fun=count)
  plot(NULL, xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1, main=df$BIOME_NAME[1])
  plot(centr_rose)
}
dev.off()

############