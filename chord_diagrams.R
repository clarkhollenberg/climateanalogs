# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
source("../GitRepos/climateanalogs/func_matrixProcessing.R")
source("../GitRepos/climateanalogs/by_biome_chord_matrix_gen.R")
source("../GitRepos/climateanalogs/by_biome_chord_matrix_gen_mta.R")

#biome master chord diagram
#######################################
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>% cleanInput(., biome=T)
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>% cleanInput(., biome=T)
PA_biome_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% cleanInput(., biome=T)
PA_biome_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% cleanInput(., biome=T)
plotChord_compare(biome_2C_flowMatrix, biome_4C_flowMatrix, PA_biome_2C_flowMatrix, PA_biome_4C_flowMatrix,
                  file="Figures/Chords/biome_chord_diag.pdf", title="Biome flux from current to ", textSize = 2.5)

#creating chord diagrams at the ecoregion level for each biome
#lists generated from source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/chord_diagram_biome_chrdMat_gen.R")
#########################################################
global_biome_2C_chrdMatrices<-lapply(global_biome_2C_chrdMatrices, as.data.frame)
global_biome_4C_chrdMatrices<-lapply(global_biome_4C_chrdMatrices, as.data.frame)
PA_biome_2C_chrdMatrices<-lapply(PA_biome_2C_chrdMatrices, as.data.frame)
PA_biome_4C_chrdMatrices<-lapply(PA_biome_4C_chrdMatrices, as.data.frame)

#ecoregion level biome chords
####################################
for (i in c(1:5))
{
  print(i)
  plotChord_compare(global_biome_2C_chrdMatrices[[i]], global_biome_4C_chrdMatrices[[i]], 
                    PA_biome_2C_chrdMatrices[[i]], PA_biome_4C_chrdMatrices[[i]],
                    file=paste0("Figures/Chords/EcoLevelBiomeFull/", gsub(" ", "", as.character(LUT_biome$BIOME_NAME[i]), fixed = TRUE), ".pdf"), title=as.character(LUT_biome$BIOME_NAME[i]),
                    biomeSub=T, trim=T)
    plotChord_compare(global_biome_2C_chrdMatrices[[i]], global_biome_4C_chrdMatrices[[i]], 
                             PA_biome_2C_chrdMatrices[[i]], PA_biome_4C_chrdMatrices[[i]],
                             file=paste0("Figures/Chords/EcoLevelBiomeMask/", gsub(" ", "", as.character(LUT_biome$BIOME_NAME[i]), fixed = TRUE), "_interCD.pdf"), title=as.character(LUT_biome$BIOME_NAME[i]),
                            biomeSub=T, trim=T, mask=T, maskval="eco")
    plotChord_compare(global_biome_2C_chrdMatrices[[i]], global_biome_4C_chrdMatrices[[i]], 
                      PA_biome_2C_chrdMatrices[[i]], PA_biome_4C_chrdMatrices[[i]],
                      file=paste0("Figures/Chords/EcoLevelBiomeMask/", gsub(" ", "", as.character(LUT_biome$BIOME_NAME[i]), fixed = TRUE), "_intraCD.pdf"), title=as.character(LUT_biome$BIOME_NAME[i]),
                      biomeSub=T, trim=T, mask=T, maskval="biome")
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
plotChord_compare<-function(matrix2C, matrix4C, matrix2Cpa, matrix4Cpa, file, title, textSize=1.5, text=T, biomeSub=F, trim=F, mask=F, maskval="biome")
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
    par(cex.main=3.6)
    layout(mat, widths=c(1, 1), heights = c(4, 4, 1))
  }
  
  circos.clear()
  circos.par(canvas.xlim=c(-1, 1), canvas.ylim=c(-1,1), start.degree = 90, 
             track.margin = c(-0.1, 0.1), gap.degree = 0, points.overflow.warning = FALSE)
  ##############################################################################################
  chordPlot<-function(color, data)
  {
    #change the matrix columns from eco/bio names to eco/bio ids
    data<-matNamesToNum(data)
    #if we want to only label the sectors with >1% of total area, then create index to mask these
    if (trim)
    {trInd<-trimInd(data, 0.01)}
    #make adjacency list
    data<-pipeToLongFormat(data)
    #create vector defining transparency for each link
    transpVec=vector(length=nrow(data))
    transpVec[]=0.25
    if (mask==T)
    {
      if (maskval=="biome") #set interbiome related links to high transparency
      {transpVec[data$rowname %in% paste0("B", 1:16) | data$key %in% paste0("B", 1:16)] = 1}
      if (maskval=="eco")  #set intrabiome related links to high transparency
      {transpVec[data$rowname %notin% paste0("B", 1:16) & data$key %notin% paste0("B", 1:16)] = 1}
    }
    
    chordDiagram(x=data,
                 annotationTrack = "grid",
                 grid.col = color,
                 transparency = transpVec,
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
                {circos.text(mean(xlim), ylim[1] + 2, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)} 
        else{
          if (sector.name %in% trInd)
                  {circos.text(mean(xlim), ylim[1] + 2, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)}
        }
      }
      # circos.axis(h = "top", labels=F, major.tick.percentage = 10, sector.index = sector.name, track.index = 1)
    }, bg.border = NA)
  }
  
  par(mar=c(1.2, 1, 1.2, 1))
  #plot
  chordPlot(col2C, data=matrix2C)
    if (biomeSub)
    {title(main=paste0(as.character(LUT_biome$BIOME_NAME[i]), " +2C"))}
    else
    {title(main="a.", line=-2, adj=0)}
  #plot
  chordPlot(col4C, data=matrix4C)
    if (biomeSub)
    {title(main=paste0(as.character(LUT_biome$BIOME_NAME[i]), " +4C"))}
    else
    {title(main="b.", line=-2, adj=0)}
  #PA chords
    #plot
    chordPlot(col2Cpa, matrix2Cpa)
      if (biomeSub)
      {title(main=paste0("PA ", as.character(LUT_biome$BIOME_NAME[i]), " +2C"))}
      else
      {title(main="c.", line=-2, adj=0)}
    #plot
    chordPlot(col4Cpa, matrix4Cpa)
      if (biomeSub)
      {title(main=paste0("PA ", as.character(LUT_biome$BIOME_NAME[i]), " +4C"))}
      else
      {title(main="d.", line=-2, adj=0)}
    #add legend
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  if (biomeSub)
  {
    par(cex=1.2)
    if (trim)
    { #take ecoids above threshold
      trimInv<-function(data){return(colnames(data)[colnames(data) %in% trimInd(data, 0.01)])}
      names=c(trimInv(matrix2C), trimInv(matrix4C), trimInv(matrix2Cpa), trimInv(matrix4Cpa))
      legeco=subset(LUT_plus[-848,], econame %in% names)
      trimInv<-function(data){return(colnames(data)[colnames(data) %in% trimInd(data, 0.005)])}
      names=c(trimInv(matrix2C), trimInv(matrix4C), trimInv(matrix2Cpa), trimInv(matrix4Cpa))
      legbio=subset(LUT_biome, BIOME_NAME %in% names)
      }
    else{legeco=subset(LUT_plus[-848,], econame %in% c(colnames(matrix2C), colnames(matrix4C), colnames(matrix2Cpa), colnames(matrix4Cpa)))
    legbio=subset(LUT_biome, BIOME_NAME %in% c(colnames(matrix2C), colnames(matrix4C), colnames(matrix2Cpa), colnames(matrix4Cpa)))}
    legend("topleft", legend=c(paste0(legeco$ECO_ID, ". ",legeco$econame), paste0("B", legbio$BIOME_ID, ". ", legbio$BIOME_NAME)), 
           ncol=3, fill=c(as.character(legeco$color), as.character(legbio$BIOME_COLOR)))
  }
  else
  {
    par(cex=0.66)
    leg=lapply(colnames(matrix2C), FUN=function(x){paste0("B", subset(LUT_biome, BIOME_NAME==x)$BIOME_ID, ". ", x)})
    legend("topleft", legend=leg, ncol=3, fill=as.character(subset(LUT_biome, BIOME_NAME %in% colnames(matrix2C))$BIOME_COLOR), cex=1.95)
  }
  dev.off()
}
