source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/matrixProcessing_func.R")
#load Biome LUT
LUT_biome<-read.csv("Tables/LUT_biome.csv")
LUT_plus<-read.csv("Tables/LUT_plus.csv")
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>%cleanInput(biome=T)%>%transitionMatrix()
now_2C_flowMatrix<-read.csv("TransitionMat/now-2C_flow_matrix.csv") %>%cleanInput()%>%transitionMatrix()
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>%cleanInput(biome=T)
now_4C_flowMatrix<-read.csv("TransitionMat/now-4C_flow_matrix.csv")  %>%cleanInput()
#PA data
PA_biome_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% cleanInput(biome=T)%>%transitionMatrix()
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv")  %>% cleanInput()
PA_biome_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% cleanInput(biome=T)
PA_eco_4C_flowMatrix<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv")
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv")

biome_2C_flowMatrix["No analog", "No analog"]<-1
bio_2C_markovMat<-new("markovchain", states=colnames(biome_2C_flowMatrix), byrow=T, transitionMatrix=as.matrix(biome_2C_flowMatrix), name="+2C Biome Movement")

t<-steadyStates(bio_2C_markovMat)
now_2C_flowMatrix["No analog"]<-NULL
now_2C_flowMatrix<-now_2C_flowMatrix[colnames(now_2C_flowMatrix),]
now_2C_flowMatrix<-now_2C_flowMatrix[rowSums(now_2C_flowMatrix)!=0]
now_2C_flowMatrix<-now_2C_flowMatrix[colnames(now_2C_flowMatrix),]
now_2C_flowMatrix<-transitionMatrix(now_2C_flowMatrix)
now_2C_markovMat<-new("markovchain", states=colnames(now_2C_flowMatrix), byrow=T, transitionMatrix=as.matrix(now_2C_flowMatrix), name="+2C Eco Movement noanalog.rm=T")
temp<-steadyStates(now_2C_markovMat)

out<-lapply(global_biome_2C_transMatrices, rmZeroRows)%>%lapply(., transitionMatrix)%>%lapply(., genMarkov)
genMarkov<-function(data, nm)
{
  return(new("markovchain", states=colnames(data), byrow=T, transitionMatrix=as.matrix(data), name=nm))
}