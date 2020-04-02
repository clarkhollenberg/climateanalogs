# methods to create and compare transition matrices
# by biome at the ecoregion level
library(corrplot)
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/matrixProcessing_func.R")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/func_byBiomeTransMatGen.R")
###################
#load Biome LUT
LUT_biome<-read.csv("Tables/LUT_biome.csv")
LUT_plus<-read.csv("Tables/LUT_plus.csv")
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>%cleanInput(biome=T)
now_2C_flowMatrix<-read.csv("TransitionMat/now-2C_flow_matrix.csv")  %>%cleanInput()
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>%cleanInput(biome=T)
now_4C_flowMatrix<-read.csv("TransitionMat/now-4C_flow_matrix.csv")  %>%cleanInput()
#PA data
PA_biome_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% cleanInput(biome=T)
PA_biome_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% cleanInput(biome=T)

#trim PA matrices to have the same rows and columns as global comparison
#corrDiagrams
#######################################################################
PA_eco_4C_flowMatrix<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv")
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv")
colnames(PA_eco_2C_flowMatrix)<-LUT_plus$econame[-849]  #take econames minus the insuff.data
rownames(PA_eco_2C_flowMatrix)<-colnames(PA_eco_2C_flowMatrix)
colnames(PA_eco_4C_flowMatrix)<-LUT_plus$econame[-849]  #take econames minus the insuff.data
rownames(PA_eco_4C_flowMatrix)<-colnames(PA_eco_4C_flowMatrix)
PA_eco_2C_flowMatrix<-PA_eco_2C_flowMatrix[,colnames(now_2C_flowMatrix)]%>%.[colnames(.),]
PA_eco_4C_flowMatrix<-PA_eco_4C_flowMatrix[,colnames(now_4C_flowMatrix)]%>%.[colnames(.),]

global_biome_2C_transMatrices<-generateByBiomeList(now_2C_flowMatrix, biome_2C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)]) %>%lapply(., transitionMatrix)
PA_biome_2C_transMatrices<-generateByBiomeList(PA_eco_2C_flowMatrix, biome_2C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)]) %>%lapply(., transitionMatrix)
global_biome_4C_transMatrices<-generateByBiomeList(now_4C_flowMatrix, biome_4C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)]) %>%lapply(., transitionMatrix)
PA_biome_4C_transMatrices<-generateByBiomeList(PA_eco_4C_flowMatrix, biome_4C_flowMatrix, LUT_biome$BIOME_ID[c(1:13)]) %>%lapply(., transitionMatrix)

#create CorrDiagrams
pdf(file="Figures/CorrDiag/Biome_prob_corrdiag.pdf", 20, 16)
par(mfrow=c(2, 3))
for (i in c(1:8, 10:13))
{
  print(i)
  tbase=LUT_biome$BIOME_NAME[i]
  temp=PA_biome_2C_transMatrices[[i]]-global_biome_2C_transMatrices[[i]]
  temp<-matNamesToNum(temp)
  corrplot(as.matrix(matNamesToNum(global_biome_2C_transMatrices[[i]])), mar= c(1, 2, 4, 2), tl.cex=0.8, method="color", title=paste0(tbase, " global +2C"), addgrid.col = "gray85")
  corrplot(as.matrix(matNamesToNum(PA_biome_2C_transMatrices[[i]])),mar= c(1, 2, 4, 2), tl.cex=0.8, method="color", title=paste0(tbase, " PA +2C"), addgrid.col = "gray85")
  corrplot(as.matrix(temp), method="color", mar= c(2, 2, 6, 2), tl.cex=0.8, title=paste0(tbase, " diff +2C"), addgrid.col = "gray85")
  temp=PA_biome_4C_transMatrices[[i]]-global_biome_4C_transMatrices[[i]]
  temp<-matNamesToNum(temp)
  corrplot(as.matrix(matNamesToNum(global_biome_4C_transMatrices[[i]])), mar= c(1, 2, 4, 2), tl.cex=0.8, method="color", title=paste0(tbase, " global +4C"), addgrid.col = "gray85")
  corrplot(as.matrix(matNamesToNum(PA_biome_4C_transMatrices[[i]])),mar= c(1, 2, 4, 2), tl.cex=0.8, method="color", title=paste0(tbase, " PA +4C"), addgrid.col = "gray85")
  corrplot(as.matrix(temp), method="color", mar= c(1, 2, 4, 2), tl.cex=0.8, title=paste0(tbase, " diff +4C"), addgrid.col = "gray85")
}
dev.off()


