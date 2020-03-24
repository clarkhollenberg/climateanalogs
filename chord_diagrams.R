# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/func_matrixProcessing.R")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/chord_diagram_biome_chrdMat_gen.R")

#biome master chord diagram
#######################################
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>% cleanInput(., biome=T)
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>% cleanInput(., biome=T)
PA_biome_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% cleanInput(., biome=T)
PA_biome_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% cleanInput(., biome=T)
plotChord_compare(biome_2C_flowMatrix, biome_4C_flowMatrix, PA_biome_2C_flowMatrix, PA_biome_4C_flowMatrix,
                  file="Figures/Chords/biome_chord_comp.pdf", title="Biome flux from current to ")

#creating chord diagrams at the ecoregion level for each biome
#lists generated from source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/chord_diagram_biome_chrdMat_gen.R")
#########################################################
global_biome_2C_chrdMatrices<-lapply(global_biome_2C_chrdMatrices, as.data.frame)
global_biome_4C_chrdMatrices<-lapply(global_biome_4C_chrdMatrices, as.data.frame)
PA_biome_2C_chrdMatrices<-lapply(PA_biome_2C_chrdMatrices, as.data.frame)
PA_biome_4C_chrdMatrices<-lapply(PA_biome_4C_chrdMatrices, as.data.frame)

#ecoregion level biome chords
####################################
for (i in c(2))
{
  print(i)
    plotChord_compare(global_biome_2C_chrdMatrices[[i]], global_biome_4C_chrdMatrices[[i]], 
                             PA_biome_2C_chrdMatrices[[i]], PA_biome_4C_chrdMatrices[[i]],
                             file=paste0("Figures/Chords/BiomeLevelFull/Biome_chord_", i, ".pdf"), title=as.character(LUT_biome$BIOME_NAME[i]),
                            biomeSub=T, trim=T)
}

setChordColor<-function(data, biomeSub=F)
{
  if (biomeSub)
  {
    #colors for ecoregions and add those for biomes
    return(c(as.character(subset(LUT_plus, econame %in% colnames(data)[colnames(data)!="No analog"])$color), 
             as.character(subset(LUT_biome, BIOME_NAME %in% colnames(data))$BIOME_COLOR)))
  }
  else
  {
    return(as.character(subset(LUT_biome, BIOME_NAME %in% colnames(data))$BIOME_COLOR))
  }
}
###################################################################################################################################
plotChord_compare<-function(matrix2C, matrix4C, matrix2Cpa, matrix4Cpa, file, title, textSize=1.5, text=F, biomeSub=F, trim=F)
{
  #set colors (biomeSub indicated that we need to check for ecoregion colors in addition to biome)
  col2C<-setChordColor(matrix2C, biomeSub=biomeSub)
  col4C<-setChordColor(matrix4C, biomeSub=biomeSub)
  col2Cpa<-setChordColor(matrix2Cpa,  biomeSub=biomeSub)
  col4Cpa<-setChordColor(matrix4Cpa,  biomeSub=biomeSub)
  
  pdf(file, 20, 20)
  mat=matrix(c(1, 2, 3, 4, 5, 5), nrow=3, ncol=2, byrow=T)
  if (biomeSub)
  {par(cex.main=2)
    layout(mat, widths=c(1, 1), heights = c(4, 4, 3.4))}
  else
  {
    par(cex.main=2)
    layout(mat, widths=c(1, 1), heights = c(4, 4, 1))
  }
  
  circos.clear()
  circos.par(canvas.xlim=c(-1, 1), canvas.ylim=c(-1,1), start.degree = 90, 
             track.margin = c(-0.1, 0.1), gap.degree = 0, points.overflow.warning = FALSE)
  chordPlot<-function(color, data, textSize, text, trim=F)
  {
    data<-matNamesToNum(data)
    if (trim)
    {trimInd<-trimInd(data, 0.01)}
    data<-pipeToLongFormat(data)
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
      if (text)
      { 
        if (trim==F)
                {circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)} 
        else{
          if (sector.name %in% trimInd)
                  {circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)}
        }
      }
      # circos.axis(h = "top", labels=F, major.tick.percentage = 10, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
  }
  
  par(mar=c(1.2, 1, 1.2, 1))
  #plot
  chordPlot(col2C, data=matrix2C, textSize=textSize, text=T, trim)
  if (biomeSub)
  {title(main=paste0(as.character(LUT_biome$BIOME_NAME[i]), " +2C"))}
  else
  {title(main=paste0(title, "+2C"), cex=2)}
  #plot
  chordPlot(col4C, data=matrix4C, textSize=textSize, text=T, trim)
  if (biomeSub)
  {title(main=paste0(as.character(LUT_biome$BIOME_NAME[i]), " +4C"))}
  else
  {title(main=paste0(title, " +4C"), cex=2)}
  #PA chords
    #plot
    chordPlot(col2Cpa, matrix2Cpa, textSize=textSize, text=T, trim)
    if (biomeSub)
    {title(main=paste0("PA ", as.character(LUT_biome$BIOME_NAME[i]), " +2C"))}
    else
    {title(main=paste0("PA ", title, " +2C"), cex=2)}
    #plot
    chordPlot(col4Cpa, matrix4Cpa, textSize=textSize, text=T, trim)
    if (biomeSub)
    {title(main=paste0("PA ", as.character(LUT_biome$BIOME_NAME[i]), " +4C"))}
    else
    {title(main=paste0("PA ", title, " +4C"), cex=2)}
  
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  if (biomeSub)
  {
    print("hi")
    par(cex=1.2)
    legeco=subset(LUT_plus[-848,], econame %in% c(colnames(matrix2C), colnames(matrix4C), colnames(matrix2Cpa), colnames(matrix4Cpa)))
    legbio=subset(LUT_biome, BIOME_NAME %in% c(colnames(matrix2C), colnames(matrix4C), colnames(matrix2Cpa), colnames(matrix4Cpa)))
    legend("topleft", legend=c(paste0(legeco$ECO_ID, ". ",legeco$econame), paste0("B", legbio$BIOME_ID, ". ", legbio$BIOME_NAME)), 
           ncol=3, fill=c(as.character(legeco$color), as.character(legbio$BIOME_COLOR)))
  }
  else
  {
    leg=lapply(colnames(matrix2C), FUN=function(x){paste0("B", subset(LUT_biome, BIOME_NAME==x)$BIOME_ID, ". ", x)})
    legend("topleft", legend=leg, ncol=3, fill=as.character(subset(LUT_biome, BIOME_NAME %in% colnames(matrix2C))$BIOME_COLOR), cex=1.95)
  }
  dev.off()
}
