# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(circlize)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/func_matrixProcessing.R")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/by_biome_chord_matrix_gen.R")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/by_biome_chord_matrix_gen_mta.R")


MTA_2C_chordMatrices<-lapply(MTA_2C_chordMatrices, as.data.frame)
MTA_4C_chordMatrices<-lapply(MTA_2C_chordMatrices, as.data.frame)


#ecoregion level biome chords
####################################
for (i in c(2:5, 7:8, 10:13))
{
  print(i)
  plotChord_compare(MTA_2C_chordMatrices[[i]], MTA_4C_chordMatrices[[i]], 
                    file=paste0("Figures/Chords/EcoLevelBiomeMTA/", gsub(" ", "", as.character(LUT_biome$BIOME_NAME[i]), fixed = TRUE), ".pdf"), title=as.character(LUT_biome$BIOME_NAME[i]),
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
plotChord_compare<-function(matrix2C, matrix4C, file, title, textSize=1.5, text=T, biomeSub=F, trim=F, mask=F, maskval="biome")
{
  #set colors (biomeSub indicated that we need to check for ecoregion colors in addition to biome)
  col2C<-setChordColor(matrix2C, biomeSub=biomeSub)
  col4C<-setChordColor(matrix4C, biomeSub=biomeSub)

  pdf(file, 20, 12)
  mat=matrix(c(1, 2, 3, 3), nrow=2, ncol=2, byrow=T)
  if (biomeSub)
  {par(cex.main=2)
    layout(mat, widths=c(1, 1), heights = c(4, 3.4))}
  else
  {
    par(cex.main=2)
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
    {trInd<-trimInd(abs(data), 0.02)}
    #make adjacency list
    data<-pipeToLongFormat(data)
    #set colors for links based on positive/negative deltaMTA
    linkcol=data$value>0
    linkcol[linkcol==T]<-"blue"
    linkcol[linkcol==F]<-"red"
    #now that we have set colors, change to abs()
    data$value=abs(data$value)
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
                 col = linkcol,
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
                {circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)} 
        else{
          if (sector.name %in% trInd)
                  {circos.text(mean(xlim), ylim[1] + 1.5, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), cex=textSize)}
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
    {title(main=paste0(title, "+2C"), cex=2)}
  #plot
  chordPlot(col4C, data=matrix4C)
    if (biomeSub)
    {title(main=paste0(as.character(LUT_biome$BIOME_NAME[i]), " +4C"))}
    else
    {title(main=paste0(title, " +4C"), cex=2)}
  #add legend
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  if (biomeSub)
  {
    par(cex=1.2)
    if (trim)
    { #take ecoids above threshold
      trimInv<-function(data){return(colnames(data)[colnames(data) %in% trimInd(abs(data), 0.02)])}
      names=c(trimInv(matrix2C), trimInv(matrix4C))
      legeco=subset(LUT_plus[-848,], econame %in% names)
      trimInv<-function(data){return(colnames(data)[colnames(data) %in% trimInd(abs(data), 0.01)])}
      names=c(trimInv(matrix2C), trimInv(matrix4C))
      legbio=subset(LUT_biome, BIOME_NAME %in% names)
      }
    else{legeco=subset(LUT_plus[-848,], econame %in% c(colnames(matrix2C), colnames(matrix4C)))
    legbio=subset(LUT_biome, BIOME_NAME %in% c(colnames(matrix2C), colnames(matrix4C)))}
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
