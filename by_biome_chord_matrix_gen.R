# methods to create and compare transition matrices
# by biome at the ecoregion level
source("../GitRepos/climateanalogs/func_byBiomeTransMatGen.R")
###################
#load data for chord diagrams
LUT_biome<-read.csv("Tables/LUT_biome.csv")
LUT_plus<-read.csv("Tables/LUT_plus.csv")
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>%cleanInput(biome=T)
now_2C_flowMatrix<-read.csv("TransitionMat/now-2C_flow_matrix.csv")  %>%cleanInput()
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>%cleanInput(biome=T)
now_4C_flowMatrix<-read.csv("TransitionMat/now-4C_flow_matrix.csv")  %>%cleanInput()
#PA data
PA_biome_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% cleanInput(biome=T)
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv")  %>% cleanInput()
PA_biome_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% cleanInput(biome=T)
PA_eco_4C_flowMatrix<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv")  %>% cleanInput()

global_biome_2C_chordMatrices<-generateByBiomeList(now_2C_flowMatrix, biome_2C_flowMatrix, LUT_biome$BIOME_ID[-c(14,15,16,17)]) #cut out biomes and No analog
PA_biome_2C_chordMatrices<-generateByBiomeList(PA_eco_2C_flowMatrix, PA_biome_2C_flowMatrix, LUT_biome$BIOME_ID[-c(14,15,16,17)]) #cut out biomes and No analog
global_biome_4C_chordMatrices<-generateByBiomeList(now_4C_flowMatrix, biome_4C_flowMatrix, LUT_biome$BIOME_ID[-c(14,15,16,17)]) #cut out biomes and No analog
PA_biome_4C_chordMatrices<-generateByBiomeList(PA_eco_4C_flowMatrix, PA_biome_4C_flowMatrix, LUT_biome$BIOME_ID[-c(14,15,16,17)]) #cut out biomes and No analog
