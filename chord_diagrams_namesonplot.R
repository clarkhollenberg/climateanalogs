

# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
setwd("~/Insync/clark.hollenberg@gmail.com/Google Drive/Analogs")
load("chord.RData")

df<-data.frame("BIOME_ID"=1:17)
LUT_biome<-merge(df, LUT_plus, by="BIOME_ID")
LUT_biome<-data.frame("BIOME_ID"= unique(LUT_biome$BIOME_ID), 
                      "BIOME_NAME"=unique(LUT_biome$BIOME_NAME), "BIOME_COLOR"= unique(LUT_biome$biome_color))
write.csv(LUT_biome, "Tables/LUT_biome.csv")

biome_2C_transMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>% cleanInput(., biome=T)%>%transitionMatrix()
biome_4C_transMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>% cleanInput(., biome=T)%>%transitionMatrix()
write.csv(biome_2C_transMatrix, "TransitionMat/biome_2C_transMatrix.csv")
write.csv(biome_4C_transMatrix, "TransitionMat/biome_4C_transMatrix.csv")
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>% cleanInput(., biome=T) %>%
                      pipeToLongFormat()
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>% cleanInput(., biome=T) %>%
                        pipeToLongFormat()
PA_bio_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv", header = T, row.names = 1) %>% cleanInput(., biome=T) %>%
                       pipeToLongFormat()
PA_bio_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv", header = T, row.names = 1) %>% cleanInput(., biome=T) %>%
                       pipeToLongFormat()

#remove values where row sums are 0 and map names to rows/columns
cleanInput<-function(data, reflexive =F, biome=F, row=F)
{
  #remove out fluxes that return to the same sector - ie set diagonal=0 (optional, default is F)
  if (reflexive==T)
  {
    diag(data)<-0
  }
  if (biome)
  {
    colnames(data)<-LUT_biome$BIOME_NAME[-17]  #take bionames minus the insuff.data
    rownames(data)<-colnames(data)
    data<-data[-c(9, 14, 15)]   #remove water biomes and r&i
    data<-data[-c(9, 14, 15), ]
  }
  else
  {
    colnames(data)<-LUT_plus$econame[-849]  #take econames minus the insuff.data
    rownames(data)<-colnames(data)
  }
  data[is.na(data)]<-0
  
  #to just remove where rows=0 (for the transition matrix this is useful - do not use for chord diagrams) --this could return an asymmetrical matrix
  if (row==T)
  {
    rows<-rowSums(data)
    #remove columns and rows with where ecoregion is not involved in giving or receiving area
    data<-data[(rows!=0), ]
  }
  else
  {
    cols<-colSums(data)
    rows<-rowSums(data)
    #remove columns and rows with where ecoregion is not involved in giving or receiving area
    data[, (cols==0 & rows==0)]<-NULL  
    data<-data[colnames(data), ]  #make the matrix symmetrical
  }

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

#remove all ecoregions below a minimum % of total area in circle (must be lower before and after)
trimMat<-function(data, minPerc)
{
  if (class(data)!="numeric")
  {
    totArea<-sum(colSums(data))
    temp<-(colSums(data)+rowSums(data))/totArea < minPerc
    data[, temp]<-NULL
    data<-data[colnames(data), ]  #make the matrix symmetrical
    return(data)
  }
  
}

#creating chord diagrams at the ecoregion level for each biome
#load dataframe lists from code in transition_matrix_comparison.R
#########################################################
temp<-trimMat(global_biome_2C_transMatrices[[2]], 0.01)
global_biome_2C_transMatrices_chrd<-lapply(global_biome_2C_transMatrices, as.data.frame)%>%
                                    lapply(., pipeToLongFormat)
global_biome_4C_transMatrices_chrd<-lapply(global_biome_4C_transMatrices, as.data.frame)%>%
                                    lapply(., pipeToLongFormat)
PA_biome_2C_transMatrices_chrd<-lapply(PA_biome_2C_transMatrices, as.data.frame)%>%
                            lapply(., pipeToLongFormat)
PA_biome_4C_transMatrices_chrd<-lapply(PA_biome_4C_transMatrices, as.data.frame)%>%
                                lapply(., pipeToLongFormat)
for (i in c(1:8, 10:13))
{
  print(i)
    plotChord_compareByBiome(global_biome_2C_transMatrices_chrd[[i]], global_biome_4C_transMatrices_chrd[[i]], 
                             PA_biome_2C_transMatrices_chrd[[i]], PA_biome_4C_transMatrices_chrd[[i]],
                             file=paste0("Figures/Chords/BiomeLevelFull/Biome_chord_", i, ".pdf"), title=as.character(LUT_biome$BIOME_NAME[i]), PA=T)
}

#PA fluxes
##############
PA_eco_2C_flow<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv") %>% cleanInput() %>% trimMat(., 25000)%>% pipeToLongFormat()
PA_eco_4C_flow<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv")%>% cleanInput() %>% trimMat(., 25000) %>% pipeToLongFormat()
eco_2C_flow<-read.csv("TransitionMat/now-2C_flow_matrix.csv")%>% cleanInput() %>% trimMat(., 200000)%>% pipeToLongFormat()
eco_4C_flow<-read.csv("TransitionMat/now-4C_flow_matrix.csv")%>% cleanInput() %>% trimMat(., 200000) %>% pipeToLongFormat()
plotChord_compare(eco_2C_flow, eco_4C_flow, PA_eco_2C_flow, PA_eco_4C_flow, "Figures/ecorgn_global_chord.pdf", "flux from current to ", 0.8, PA=T)

#subset ecoregion plots
westUSA_2Cflow_matrix<-read.csv("Outputs/westUSA_ecorgn2C_flux.csv") %>% cleanSubsetInput() %>% pipeToLongFormat()
westUSA_4Cflow_matrix<-read.csv("Outputs/westUSA_ecorgn4C_flux.csv") %>% cleanSubsetInput() %>% pipeToLongFormat()


setChordColor<-function(data, biomeSub=F)
{
  if (biomeSub)
  {
    #colors for ecoregions and add those for biomes
    return(c(as.character(subset(LUT_plus, econame %in% data$rowname[data$rowname!="No analog"])$color), 
             as.character(subset(LUT_biome, BIOME_NAME %in% data$rowname)$BIOME_COLOR)))
  }
}
  
plotChord_compareByBiome<-function(matrix2C, matrix4C, matrix2Cpa=NULL, matrix4Cpa=NULL, file, title, textSize=0.8, PA=F)
{

  #set color palettes
  col2C<-setChordColor(matrix2C, biomeSub = T)
  col4C<-setChordColor(matrix4C, biomeSub = T)
  if (PA)
  {
    col2Cpa<-setChordColor(matrix2Cpa, biomeSub = T)
    col4Cpa<-setChordColor(matrix4Cpa, biomeSub = T)
  }
  #settings for 4 vs 2 plots
  if (PA){pdf(file, 20, 20)
    par(mfrow=c(2, 2), mar=c(2, 1, 2, 1))}
  else{pdf(file, 20, 12)
    par(mfrow=c(1, 2), mar=c(2, 1, 2, 1))}
  
  circos.clear()
  circos.par(canvas.xlim=c(-1.3, 1.3), canvas.ylim=c(-1.25, 1.25), start.degree = 90, 
             track.margin = c(-0.1, 0.1), gap.degree = 0, points.overflow.warning = FALSE)
  chordPlot<-function(color, title, data, textSize)
  {
    print(color)
    chordDiagram(x=data,
                 annotationTrack = "grid",
                 grid.col = color,
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
      circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)
      circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
    title(main=title)
  }
  
  chordPlot(col2C, title=paste0(title, "+2C"), data=matrix2C, textSize=textSize)
  chordPlot(col4C, title=paste0(title, "+4C"), data=matrix4C, textSize=textSize)
  if (PA)
  {
    chordPlot(col2Cpa, title=paste0("PA ", title, "+2C"), matrix2Cpa, textSize=textSize)
    chordPlot(col4Cpa, title=paste0("PA ", title, "+4C"), matrix4Cpa, textSize=textSize)
  }
  dev.off()
}


plotChord_compare<-function(matrix2C, matrix4C, matrix2Cpa=NULL, matrix4Cpa=NULL, file, title, textSize=0.8, biome=F, PA=F)
{
  #set colors for biomes
  if (biome)
  {
    col2C<-as.character(subset(LUT_biome, BIOME_NAME %in% matrix2C$rowname)$BIOME_COLOR)
    col4C<-as.character(subset(LUT_biome, BIOME_NAME %in% matrix4C$rowname)$BIOME_COLOR)
    if(PA)
    {
      col2Cpa<-as.character(subset(LUT_biome, BIOME_NAME %in% matrix2Cpa$rowname)$BIOME_COLOR)
      col4Cpa<-as.character(subset(LUT_biome, BIOME_NAME %in% matrix4Cpa$rowname)$BIOME_COLOR)
    }
  }
  else #colors for ecoregions
  {
    col2C<-as.character(subset(LUT_plus, econame %in% matrix2C$rowname)$color)
    col4C<-as.character(subset(LUT_plus, econame %in% matrix4C$rowname)$color)
    if(PA)
    {
      col2Cpa<-as.character(subset(LUT_plus, econame %in% matrix2Cpa$rowname)$color)
      col4Cpa<-as.character(subset(LUT_plus, econame %in% matrix4Cpa$rowname)$color)
    }
  }
  
  #settings for 4 vs 2 plots
  if (PA){pdf(file, 20, 20)
    par(mfrow=c(2, 2), mar=c(2, 1, 2, 1))}
  else{pdf(file, 20, 12)
    par(mfrow=c(1, 2), mar=c(2, 1, 2, 1))}
  
  circos.clear()
  circos.par(canvas.xlim=c(-1.3, 1.3), canvas.ylim=c(-1.25, 1.25), start.degree = 90, 
             track.margin = c(-0.1, 0.1), gap.degree = 0, points.overflow.warning = FALSE)
  chordPlot<-function(color, title, data, textSize)
  {
    chordDiagram(x=data,
                 annotationTrack = "grid",
                 grid.col = color,
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
      circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)
      circos.axis(h = "top", labels=F, major.tick.percentage = 0.5, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
    title(main=title)
  }
  chordPlot(col2C, title=paste0(title, "+2C"), data=matrix2C, textSize=textSize)
  chordPlot(col4C, title=paste0(title, "+4C"), data=matrix4C, textSize=textSize)
  if (PA)
  {
    chordPlot(col2Cpa, title=paste0("PA ", title, "+2C"), matrix2Cpa, textSize=textSize)
    chordPlot(col4Cpa, title=paste0("PA ", title, "+4C"), matrix4Cpa, textSize=textSize)
  }
  dev.off()
}

plotChord_compare(biome_2C_flowMatrix, biome_4C_flowMatrix,
                  file="Figures/biome_chord_comp.pdf", title="biome flux from current to ", 0.5, biome=T)


plotChord_compare(westUSA_2Cflow_matrix, westUSA_4Cflow_matrix, "Figures/westUSA_chord.pdf", "westUSA")



#Global plots by biome
biomenames<-LUT_biome[order(LUT_biome$BIOME_ID), ]
biomenames<-biomenames[-c(9, 14, 15, 17), ]  #remove insufficient data and water biomes
biomenames$BIOME_NAME<-as.character(biomenames$BIOME_NAME)  #change from factor to character, so we can add line breaks to long titles
biomenames$BIOME_NAME[c(1:4, 7, 8, 9, 11)]<-c("Tropical & Subtropical\n Moist Broadleaf Forests", "Tropical & Subtropical Dry\n Broadleaf Forests", "	Tropical & Subtropical\n Coniferous Forests",
                                      "Temperate\n Broadleaf &\n Mixed Forests", 	"Tropical & Subtropical Grasslands,\n Savannas & Shrublands", 	
                                      "Temperate Grasslands,\n Savannas & Shrublands", "	Montane Grasslands\n & Shrublands", "Mediterranean Forests,\n Woodlands & Scrub")




