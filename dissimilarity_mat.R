###################################################################################################
#scale by total area - dissimilarity input
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv")  %>% cleanInput()
PA_eco_4C_flowMatrix<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv")  %>% cleanInput()
divTotArea<-function(data){return(data/(sum(rowSums(data))))}
diagZero<-function(x){diag(x)<-0; return(x)}
global_biome_2C_dissMatrices<-generateByBiomeList(now_2C_flowMatrix, biome_2C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)])%>%
  lapply(., divTotArea)%>%lapply(., diagZero)
PA_biome_2C_dissMatrices<-generateByBiomeList(PA_eco_2C_flowMatrix, biome_2C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)])%>%lapply(., divTotArea)%>%
  lapply(., divTotArea)%>%lapply(., diagZero)
global_biome_4C_dissMatrices<-generateByBiomeList(now_4C_flowMatrix, biome_4C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)])%>%lapply(., divTotArea)%>%
  lapply(., divTotArea)%>%lapply(., diagZero)
PA_biome_4C_dissMatrices<-generateByBiomeList(PA_eco_4C_flowMatrix, biome_4C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)])%>%lapply(., divTotArea)%>%
  lapply(., divTotArea)%>%lapply(., diagZero)
