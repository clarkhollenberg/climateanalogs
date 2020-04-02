mta_by_eco.df<-read.csv("Tables/mta_by_eco.csv")

perc_comparison_master<-read.csv("Tables/PA_perc_comparison_master.csv")

mtaEco<-function(perc)
{df<-perc/0.3
df[df>1]<-1
return(df)}

deltaMTA<-function(ecoNameRow, ecoNameCol, totMatrix, paMatrix, row, MTA=T)
{
  # don't calculate delta for self-flux
  if (ecoNameRow==ecoNameCol)
  {return(0)}
  
  TAx<-totMatrix[ecoNameRow, ecoNameCol]   #tot_area flux
  if (TAx!=0) #if no global flux than delta=0
  {
    #check that the ecoregions are in the paMatrix
    if ((ecoNameRow %in% rownames(paMatrix)) & (ecoNameCol %in% colnames(paMatrix)))
    {PAx<-paMatrix[ecoNameRow, ecoNameCol]} #PA flux
    else {PAx<-0}
    
    
    if (row==T)  #for area sending
    {
      PAi<-subset(perc_comparison_master, econame==ecoNameRow)$PA_area_current    #initial protected area
      TAi<-subset(perc_comparison_master, econame==ecoNameRow)$tot_area_current   #initial total area
        #lost area effect (MTA_f-MTA_i)
      if (MTA)
      {delta<-mtaEco((PAi-PAx)/(TAi-TAx)) - subset(mta_by_eco.df, econame==ecoNameRow)$mta_now}    #lost area effect (MTA_f-MTA_i)
      else
      {delta<-(PAi-PAx)/(TAi-TAx) - subset(perc_comparison_master, econame==ecoNameRow)$PA_perc_current}  #lost area effect (perc_f-perc_i)
      return(delta)
    }
    else  #for area receiving
    {
      PAi<-subset(perc_comparison_master, econame==ecoNameCol)$PA_area_current    #initial protected area
      TAi<-subset(perc_comparison_master, econame==ecoNameCol)$tot_area_current   #initial total area
      if (MTA)
      {delta<-mtaEco((PAi+PAx)/(TAi+TAx)) - subset(mta_by_eco.df, econame==ecoNameCol)$mta_now}   #gained area effect (MTA_f-MTA_i)
      else
      {delta<-(PAi+PAx)/(TAi+TAx) - subset(perc_comparison_master, econame==ecoNameCol)$PA_perc_current}  #gained area effect (perc_f-perc_i)
      return(delta)
    }
  }
  return(0)
}

genMTABiomeChord<-function(totMatrix, paMatrix)
{
  mtaMatrix<-totMatrix
  focalBiome<-subset(LUT_plus, econame==colnames(totMatrix)[1])$BIOME_ID
  focalEcos<- as.character(subset(LUT_plus, BIOME_ID==focalBiome)$econame)
  for (i in rownames(totMatrix))
  {
    print("i")
    print(i)
    for (j in colnames(totMatrix))
    {
      print("j")
      print(j)
      if ((i %in% focalEcos) & (j %in% focalEcos))
      {mtaMatrix[i, j]<-deltaMTA(i, j, totMatrix, paMatrix, T)+deltaMTA(i, j, totMatrix, paMatrix, F)}
      else if (i %in% focalEcos)
      {mtaMatrix[i,j]<-deltaMTA(i, j, totMatrix, paMatrix, T)}
      else if (j %in% focalEcos)
      {mtaMatrix[i,j]<-deltaMTA(i, j, totMatrix, paMatrix, F)}
    }
  }
  return(mtaMatrix)
}

MTA_2C_chordMatrices<-mapply(genMTABiomeChord, global_biome_2C_chordMatrices, PA_biome_2C_chordMatrices)
MTA_4C_chordMatrices<-mapply(genMTABiomeChord, global_biome_4C_chordMatrices, PA_biome_4C_chordMatrices)
