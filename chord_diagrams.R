

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
setwd("Documents/Analogs")



cleanSubsetInput<-function(data)
{
  colnames(data)<-LUT_plus$econame[-849]  #take econames minus the insuff.data
  rownames(data)<-colnames(data)
  data[is.na(data)]<-0
  cols<-colSums(data)
  rows<-rowSums(data)
  #remove columns and rows with where ecoregion is not involved in giving or receiving area
  data[, (cols==0 & rows==0)]<-NULL  
  data<-data[colnames(data), ]  #make the matrix symmetrical
  return(data)
}

#subset ecoregion plots
westUSA_2Cflow_matrix<-read.csv("Outputs/westUSA_ecorgn2C_flux.csv") %>% cleanSubsetInput() %>% pipeToLongFormat()
westUSA_4Cflow_matrix<-read.csv("Outputs/westUSA_ecorgn4C_flux.csv") %>% cleanSubsetInput() %>% pipeToLongFormat()

plotChord_compare<-function(matrix2C, matrix4C, file)
{
  col2C<-subset(LUT_plus, econame %in% matrix2C$rowname)$color
  col4C<-subset(LUT_plus, econame %in% matrix4C$rowname)$color
  pdf(file, 20, 12)
  circos.clear()
  par(mfrow=c(1, 2), mar=c(2, 1, 2, 1))
  circos.par(canvas.xlim=c(-1.3, 1.3), canvas.ylim=c(-1.25, 1.25), start.degree = 90, 
             track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  # Plot 2C fluxes
  chordDiagram(x=matrix2C,
               annotationTrack = "grid",
               grid.col = col2C,
               transparency = 0.25,
               directional = 1,
               direction.type = c("arrows", "diffHeight"),
               diffHeight  = -0.04,
               link.arr.type = "big.arrow",
               link.sort = TRUE,
               link.largest.ontop = TRUE)
  
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.8)
      circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
    title(main="WestUS Shifts from Current to +2C")
    #####################
    # plot 4C fluxes
    chordDiagram(x=matrix4C,
                 annotationTrack = "grid",
                 grid.col = col4C,
                 transparency = 0.25,
                 directional = 1,
                 direction.type = c("arrows", "diffHeight"),
                 diffHeight  = -0.04,
                 link.arr.type = "big.arrow",
                 link.sort = TRUE,
                 link.largest.ontop = TRUE)
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      xlim = get.cell.meta.data("xlim")
      ylim = get.cell.meta.data("ylim")
      sector.name = get.cell.meta.data("sector.index")
      circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.8)
      circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
    title(main="WestUS Shifts from Current to +4C")
    
    dev.off()
}

plotChord_compare(westUSA_2Cflow_matrix, westUSA_4Cflow_matrix, "Figures/westUSA_chord.pdf")



#Global plots by biome
biomenames<-LUT_biome[order(LUT_biome$BIOME_ID), ]
biomenames<-biomenames[-c(9, 14, 15, 17), ]  #remove insufficient data and water biomes
biomenames$BIOME_NAME<-as.character(biomenames$BIOME_NAME)  #change from factor to character, so we can add line breaks to long titles
biomenames$BIOME_NAME[c(1:4, 7, 8, 9, 11)]<-c("Tropical & Subtropical\n Moist Broadleaf Forests", "Tropical & Subtropical Dry\n Broadleaf Forests", "	Tropical & Subtropical\n Coniferous Forests",
                                      "Temperate\n Broadleaf &\n Mixed Forests", 	"Tropical & Subtropical Grasslands,\n Savannas & Shrublands", 	
                                      "Temperate Grasslands,\n Savannas & Shrublands", "	Montane Grasslands\n & Shrublands", "Mediterranean Forests,\n Woodlands & Scrub")
# color palette
biomeColor <- as.character(biomenames$BIOME_COLOR)


#preprocess input flow matrix to remove entries and add biome names
formatBiomeInput<-function(data, namesDF)
{
  data['X']<-NULL
  #remove mangroves,flooded grassland, and rock and ice biomes
  data<-data[-c(9, 14, 15)]
  data<-data[-c(9, 14, 15), ]
  
  colnames(data)<-namesDF$BIOME_NAME
  rownames(data)<-colnames(data)
  return(data)
 }

#change from adjacency matrix to adjacency list
pipeToLongFormat<-function(data)
{
  data_long <- data %>%
    rownames_to_column %>%
    gather(key = 'key', value = 'value', -rowname)
  data_long[is.na(data_long)]<-0
  return(data_long)
}

# Load datasets
biomedata_2C <- read.csv("Outputs/biome_2C_flow_matrix.csv") %>% formatBiomeInput(., biomenames) %>% pipeToLongFormat()
biomedata_4C <- read.csv("Outputs/biome_4C_flow_matrix.csv") %>% formatBiomeInput(., biomenames) %>% pipeToLongFormat()

# plot biome chord diagrams
####################################################################
pdf("Chord_diag_biomes.pdf", 20, 12)
circos.clear()
par(mfrow=c(1, 2), mar=c(2, 1, 2, 1))
circos.par(canvas.xlim=c(-1.3, 1.3), canvas.ylim=c(-1.25, 1.25), start.degree = 90, 
           track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
# Plot 2C fluxes
chordDiagram(x=biomedata_2C,
  annotationTrack = "grid",
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight  = -0.04,
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.8)
  circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
}, bg.border = NA)
title(main="Biome Shifts from Current to +2C")
#####################
# plot 4C fluxes
chordDiagram(x=biomedata_4C,
             annotationTrack = "grid",
             grid.col = mycolor,
             transparency = 0.25,
             directional = 1,
             direction.type = c("arrows", "diffHeight"),
             diffHeight  = -0.04,
             link.arr.type = "big.arrow",
             link.sort = TRUE,
             link.largest.ontop = TRUE)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=0.8)
  circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
}, bg.border = NA)
title(main="Biome Shifts from Current to +4C")

dev.off()




