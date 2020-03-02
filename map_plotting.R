#climate analogs plotting code for map figures

#plotting ecoregion comparisons
###################
##load countrylines for mapping
nationalBorders<-readOGR(dsn="InRasters/countries_shp",layer='countries')
##load statelines for mapping
uSAstatelines<-readOGR(dsn="InRasters/USA_state_shp",layer='cb_2018_us_state_5m')

#function takes raster input and adjusts colors to match eco ids
plotWColors<-function(rasterName, title, leg_x=NULL, leg_y=NULL, txtsize=NULL)
{
        df <- data.frame(unique(rasterName))
        colnames(df)<- "ECO_ID"
        df<-merge(df, LUT_plus, by="ECO_ID")
        color_palate_leg <- as.character(df$color)
        legend_names <- as.character(df$econame)
        
        rangeValues<-c((minValue(rasterName)+1):(maxValue(rasterName)+1))
        color_palate_plot<-as.character(LUT_plus$color[rangeValues])
        plot(rasterName, col=color_palate_plot, main = title, legend=FALSE)
}

#function takes raster input and adjusts colors to match biome ids
plotByBiome<-function(rasterName, title)
{
        rangeValues<-c((minValue(rasterName)+1):(maxValue(rasterName)+1))
        color_palate_plot<-as.character(LUT_plus$biome_color[rangeValues])
        plot(rasterName, col=color_palate_plot, main = title, legend=FALSE)
}


#function plots seperate color coded legend, showing ecoregions in two raster maps being compared
plotLegend<-function(rasterName1, rasterName2, rasterName3, txtsize, columns)
{
        #combine unique ecoregions from the two rasters to create a legend that represents all of them
        df1<-unique(rasterName1)
        df2<-unique(rasterName2)
        df3<-unique(rasterName3)
        df<-data.frame(unique(c(df1, df2, df3)))
        
        colnames(df)<- "ECO_ID"
        df<-merge(df, LUT_plus, by="ECO_ID")
        color_palate_leg <- as.character(df$color)
        legend_names <- as.character(df$econame)
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("topleft", legend=legend_names, cex=txtsize, ncol = columns, fill=color_palate_leg)
}

#function plots seperate color coded legend, showing ecoregions in two raster maps being compared
plotBiomeLegend<-function(rasterName1, rasterName2, rasterName3, txtsize, columns)
{
        #combine unique ecoregions from the three rasters to create a legend that represents all of them
        df1<-unique(rasterName1)
        df2<-unique(rasterName2)
        df3<-unique(rasterName3)
        df<-data.frame(unique(c(df1, df2, df3)))
        colnames(df)<- "ECO_ID"
        df<-merge(df, LUT_plus, by="ECO_ID")
        #pull unique biomes from these ecoregions and create new LUT
        df1<-data.frame(unique(df$BIOME_ID))
        colnames(df1)<- "BIOME_ID"
        df<-merge(df1, LUT_plus, by="BIOME_ID")
        #now we have the correct biomes, but need only one row per biome
        color_palate_leg <- as.character(unique(df$biome_color))
        legend_names <- as.character(unique(df$BIOME_NAME))
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("topleft", legend=legend_names, cex=txtsize, ncol = columns, fill=color_palate_leg)
}

#function to make figures as pdf comparing two ecoregion maps with defined boundary
ecorgn_compareMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
        bounds<-extent(extentVector)
        rast_current<-crop(ecorgn_rast_now, bounds)
        rast_2C<-crop(ecorast_2C_mapped, bounds)
        rast_4C<-crop(ecorast_4C_mapped, bounds)
        rast_borders<-crop(borderShp, bounds)
        
        pdf(file= filename, 28, 8)
        par(mfrow = c(1, 4))
        plotLegend(rast_current, rast_2C, rast_4C, leg_txt, leg_col)
        plotWColors(rast_current, "Current Ecoregions")
        plot(rast_borders, bg="transparent", add=TRUE)
        plotWColors(rast_2C, "+2C Ecoregions")
        plot(rast_borders, bg="transparent", add=TRUE)
        plotWColors(rast_4C, "+4C Ecoregions")
        plot(rast_borders, bg="transparent", add=TRUE)
        #plot images
        dev.off()
}

#function to plot biome comparison
biome_compareMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
        bounds<-extent(extentVector)
        rast_current<-crop(ecorgn_rast_now, bounds)
        rast_2C<-crop(ecorast_2C_mapped, bounds)
        rast_4C<-crop(ecorast_4C_mapped, bounds)
        rast_borders<-crop(borderShp, bounds)
        
        pdf(file= filename, 25, 8)
        par(mfrow = c(1, 4))
        plotBiomeLegend(rast_current, rast_2C, rast_4C, leg_txt, leg_col)
        plotByBiome(rast_current, "Current Biomes")
        plot(rast_borders, bg="transparent", add=TRUE)
        plotByBiome(rast_2C, "+2C Biomes")
        plot(rast_borders, bg="transparent", add=TRUE)
        plotByBiome(rast_4C, "+4C Biomes")
        plot(rast_borders, bg="transparent", add=TRUE)
        #plot images
        dev.off()
}

#function to make figues as pdf showing movement vectors of ecoregions from now to 2C
create_ecorgn_movementMap_PDF<-function(extentVector, filename, leg_txt, leg_col, borderShp)
{
        bounds<-extent(extentVector)
        rast_current<-crop(ecorgn_rast_now, bounds)
        rast_borders<-crop(borderShp, bounds)
        
        pdf(file= filename, 18, 10)
        par(mfrow=c(1,2))
        
        # layout(mat=matrix(c(1, 2), nrow=1, ncol=2), heights=c(1), widths=c(1, 2))
        plotLegend(rast_current, NULL, leg_txt, leg_col)
        plotWColors(rast_current, "Ecoregion Movement to +2C Locations")
        # layout(mat=matrix(c(1, 2), nrow=1, ncol=2), heights=c(1), widths=c(1, 2))
        plot(rast_borders, bg="transparent", add=TRUE)
        drawDispVectors(rast_current, extentVector)
        #plot images
        dev.off()
} 

#function to draw vectors between ecoids on selected raster area
drawDispVectors<-function(rasterName, extentVector)
{
        df<-data.frame(unique(rasterName))
        colnames(df)<-"ECO_ID"
        df<-merge(df, centr_mastr, by="ECO_ID")
        for (i in unique(rasterName))
        {
                #we can only draw an arrow if the ecoregion exists in both current and 2c conditions, there are fewer ecoregions in +2C
                if (i %in% df$ECO_ID)
                {
                        df2<-subset(df, ECO_ID==i)
                        x1<-as.numeric(df2$x_now)
                        x2<-as.numeric(df2$x_2C)
                        y1<-as.numeric(df2$y_now)
                        y2<-as.numeric(df2$y_2C)
                        
                        #only plot the arrow if it is within the raster bounds
                        if (extentVector[1]<x1 & x1<extentVector[2] & extentVector[3]<y1 & y1<extentVector[4])
                        {
                                Arrows(x1, y1, x2, y2, size=0.5)
                        }
                }
        }
}

#applying the functions to generate ecorgn_comparison plots
ecorgn_compareMap_PDF(c(-125, -105, 32, 49), "Figures/westrnUSA_ecorgns.pdf", 1.1, 2, uSAstatelines)
ecorgn_compareMap_PDF(c(-170, -140, 55, 75), "Figures/alaska_ecorgns.pdf", 1, 2, nationalBorders)
ecorgn_compareMap_PDF(c(-70, -53, -15, 5), "Figures/amazon_ecorgns.pdf", 1, 2, nationalBorders)
ecorgn_compareMap_PDF(c(-9, 20, 37, 65), "Figures/europe_ecorgns.pdf", 0.9, 1, nationalBorders)
ecorgn_compareMap_PDF(c(80, 110, 25, 45), "Figures/asia_ecorgns.pdf", 0.8, 2, nationalBorders)
ecorgn_compareMap_PDF(c(10, 40, -10, 25), "Figures/congo_ecorgns.pdf", 0.9, 2, nationalBorders)

#applying for biome comparison plots
biome_compareMap_PDF(c(-125, -105, 32, 49), "Figures/westrnUSA_biomes.pdf", 2, 1, uSAstatelines)
biome_compareMap_PDF(c(-170, -140, 55, 75), "Figures/alaska_biomes.pdf", 2, 1, nationalBorders)
biome_compareMap_PDF(c(-70, -53, -15, 5), "Figures/amazon_biomes.pdf", 1.5, 1, nationalBorders)
biome_compareMap_PDF(c(-9, 20, 37, 65), "Figures/europe_biomes.pdf", 2, 1, nationalBorders)
biome_compareMap_PDF(c(80, 110, 25, 45), "Figures/asia_biomes.pdf", 1, 1, nationalBorders)
biome_compareMap_PDF(c(10, 40, -10, 25), "Figures/congo_biomes.pdf", 1.1, 1, nationalBorders)

#maps with arrows from current to 2C
create_ecorgn_movementMap_PDF(c(-125, -105, 32, 49), 'westrnUSA_ecorgn_movement.pdf', 1.15, 1, uSAstatelines)
create_ecorgn_movementMap_PDF(c(-170, -140, 55, 75), 'alaska_ecorgns_movement.pdf', 2, 1, nationalBorders)
create_ecorgn_movementMap_PDF(c(-70, -52, -15, 5), "amazon_ecorgns_movement.pdf", 1.15, 1, nationalBorders)
create_ecorgn_movementMap_PDF(c(-9, 20, 37, 65), "europe_ecorgns_movement.pdf", 1.0, 1, nationalBorders)

#determine where ecoregions are changing globally (where the ECO_ID changes between rasters)
#######################################################
ecorgn_rast_change_2C <- ecorgn_rast_2C - ecorgn_rast_now
ecorgn_rast_change_4C <- ecorgn_rast_4C - ecorgn_rast_now  #don't use mapped versions here to preserve NAs where we aren't sure

###Maps of ecoregion shift as binary
#####
ecorgn_rast_change_2C[ecorgn_rast_change_2C[]>0 | ecorgn_rast_change_2C[]<0]<- 1
ecorgn_rast_change_4C[ecorgn_rast_change_4C[]>0 | ecorgn_rast_change_4C[]<0]<- 1

#create map of binary ecoregion change, overlaid with protected areas
areas_ecoregion_change_PDF<-function(extentVector, filename, leg_txt, borderShp)
{
        bounds<-extent(extentVector)
        #Use ecorgn_rast_now to get a gray background for the land surface (ensuring that NA data on the land isn't plotted as white)
        rast_bg<-crop(jmask, bounds)
        rast_change<-crop(ecorgn_rast_change_bin, bounds)
        rast_PA<-crop(PA_bin, bounds)
        rast_borders<-crop(borderShp, bounds)
        
        pdf(file= filename, 18, 10)
        par(mfrow=c(1,2))
        plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
        legend("topleft", legend=c("Ecoregion static", "Ecoregion shift", "Protected areas", "Protected Areas with shifting ecoregion"), cex=leg_txt, 
               ncol = 1, fill=c("light grey", "red", "yellow", "orange"))
        plot(rast_bg, col=c("light grey", "light grey"), main = "Areas of ecoregion change +2C", legend=FALSE)
        plot(rast_change, col=c("light grey", "red"),
             legend=FALSE, add=TRUE)
        plot(rast_PA, col=alpha("yellow", 0.6), legend=FALSE, add=TRUE)
        plot(rast_borders, bg="transparent", add=TRUE)
        #plot images
        dev.off()
}  
areas_ecoregion_change_PDF(c(-125, -105, 32, 49), 'westrnUSA_ecorgn_change.pdf', 2, uSAstatelines)
areas_ecoregion_change_PDF(c(-170, -140, 55, 75), 'alaska_ecorgns_change.pdf', 2, nationalBorders)
areas_ecoregion_change_PDF(c(-70, -52, -15, 5), "amazon_ecorgns_change.pdf", 2, nationalBorders)
areas_ecoregion_change_PDF(c(-9, 20, 37, 65), "europe_ecorgns_change.pdf", 2, nationalBorders)

#global plot ecoregion shift
#################
pdf(file="global_ecorgns_change.pdf", 22, 10)
par(mfrow=c(1, 2))
plot(ecorgn_rast_change_2C, col=c("light grey", "red"), main = "Areas of ecoregion change +2C", legend=FALSE)
legend("topleft", legend=c("Ecoregion static", "Ecoregion shift", "Protected areas", "Protected Areas with shift"), cex=1, 
       ncol = 1, fill=c("light grey", "red", "yellow", "orange"))
plot(PA_bin, col=alpha("yellow", 0.6), legend=FALSE, add=TRUE)
plot(nationalBorders, bg="transparent", add=TRUE)

plot(ecorgn_rast_change_4C, col=c("light grey", "red"), main = "Areas of ecoregion change +4C", legend=FALSE)
legend("topleft", legend=c("Ecoregion static", "Ecoregion shift", "Protected areas", "Protected Areas with shift"), cex=1, 
       ncol = 1, fill=c("light grey", "red", "yellow", "orange"))
plot(PA_bin, col=alpha("yellow", 0.6), legend=FALSE, add=TRUE)
plot(nationalBorders, bg="transparent", add=TRUE)
#plot images
dev.off()
#####################################

#plot comparison perc protected
#####
pdf(file="Perc_protected.pdf", 20, 8)
par(mfrow=c(1,2))
co<-colorRampPalette(c("red", "gray96", "blue"), bias=3)(25)
plot(perc_current_rast, col=co, main="Proportion of current ecoregions protected")
plot(nationalBorders, add=TRUE, legend=FALSE)
plot(perc_2C_rast, col=co, main="Proportion of +2C ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
dev.off()
#####

#plot change in perc protected
#####
pdf(file="Perc_protected_change.pdf", 20, 8)
par(mfrow=c(1, 2))
co<-colorRampPalette(c("red", "gray96", "blue"))(20)
plot(perc_change_rast_2C, col=co, xlim= c(-180, 180), zlim=c(-0.5, 0.5), main="+2C Change in proportion of ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
plot(perc_change_rast_4C, col=co, xlim= c(-180, 180), zlim=c(-0.5, 0.5), main="+4C Change in proportion of ecoregions protected")
legend("topleft", legend=c("No Analog"), fill=c("white"))
plot(nationalBorders, add=TRUE, legend=FALSE)
dev.off()
#####
