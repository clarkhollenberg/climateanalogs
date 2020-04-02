state_table_eco.df<-read.csv("Tables/state_table_eco.csv")[-849,] #remove insuff. data
trans_table_eco.df<-read.csv("Tables/trans_table_eco.csv")
perc_compare.df<-read.csv("Tables/PA_perc_comparison_master.csv")[-849,]
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/func_byBiomeTransMatGen.R")
source("~/Insync/clark.hollenberg@gmail.com/Google Drive/GitRepos/climateanalogs/func_matrixProcessing.R")       

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
       
    
kappa.stat<-function(fluxMat, stateCol_i, stateCol_f)
{
  stateCol_i<-stateCol_i/100 #transform from percents to probabilities
  stateCol_f<-stateCol_f/100 #transform from percents to probabilities
  P_o<-sum(diag(fluxMat))/sum(rowSums(fluxMat))  #observed probability of agreement (no ecoregion change)
  P_agree<-stateCol_i*stateCol_f
  ind<-as.numeric(subset(LUT_plus, econame %in% colnames(fluxMat))$ECO_ID)+1
  P_agree<-P_agree[ind] #take only the entries that are present in the flux matrix, the excluded values are 0 anyways
  P_e<-sum(stateCol_i*stateCol_f)  #expected random probability of agreement (probability of intersection based on chance)
  # P_e<-sum(P_agree*rowSums(fluxMat))/sum(rowSums(fluxMat))  
  print(P_e)
  kappa<-(P_o-P_e)/(1-P_e)
  return(kappa)
}

t<-kappa.stat(as.matrix(now_2C_flowMatrix), state_table_eco.df$perc_total_now, state_table_eco.df$perc_total_2C)
t1<-kappa.stat(as.matrix(now_4C_flowMatrix), state_table_eco.df$perc_total_now, state_table_eco.df$perc_total_4C)
t2<-kappa.stat(as.matrix(PA_eco_2C_flowMatrix), state_table_eco.df$perc_PA_now, state_table_eco.df$perc_PA_2C)
t3<-kappa.stat(as.matrix(PA_eco_4C_flowMatrix), state_table_eco.df$perc_PA_now, state_table_eco.df$perc_PA_4C)

out<-matrix(c(t, t2, t1, t3), nrow=2, ncol=2, byrow=T)
colnames(out)<-c("Global", "PA")
rownames(out)<-c("+2C", "+4C")
write.csv(out, "Tables/GlobalKappaStats.csv")


#for each biome
kappa.stat.biome<-function(transCol, areaCol)
{
  vec<-vector()
  for (i in c(1:8, 10:13))
  {
    ind<-as.numeric(subset(trans_table_eco.df, BIOME_ID==i)$ECO_ID) + 1
    print(ind)
    t<-1 - transCol[ind]/100 # the proportion that did not transition
    print(t)
    a<-areaCol[ind]  # the initial areas of transitioning ecoregions
    print(a)  
    kappa<-sum(t*a)/sum(a)
  vec<-c(vec, kappa)
  }
  return(vec)
}
#for each biome
trans.stat.biome<-function(transCol)
{
  vec<-vector()
  for (i in c(1:8, 10:13))
  {
    ind<-as.numeric(subset(trans_table_eco.df, BIOME_ID==i)$ECO_ID) + 1
    print(ind)
    t<-1 - transCol[ind]/100 # the proportion that did not transition
    ave<-mean(t)
    vec<-c(vec, ave)
  }
  return(vec)
}
trans_table_eco.df[is.na(trans_table_eco.df)]=0

kappa.biome.df<-data.frame("BIOME_ID"=c(1:8, 10:13), "BIOME_NAME"=LUT_biome$BIOME_NAME[c(1:8, 10:13)], 
                "tot_2C"=kappa.stat.biome(trans_table_eco.df$perc_trans_2C, perc_compare.df$tot_area_current),
                "PA_2C"=kappa.stat.biome(trans_table_eco.df$perc_trans_2C_PA, perc_compare.df$PA_area_current),
                "tot_4C"=kappa.stat.biome(trans_table_eco.df$perc_trans_4C, perc_compare.df$tot_area_current),
                "PA_4C"=kappa.stat.biome(trans_table_eco.df$perc_trans_4C_PA, perc_compare.df$PA_area_current))
write.csv(kappa.biome.df, "Tables/biomeKappaStats.csv", row.names=F)

trans.biome.df<-data.frame("BIOME_ID"=c(1:8, 10:13), "BIOME_NAME"=LUT_biome$BIOME_NAME[c(1:8, 10:13)], 
                           "tot_2C"=trans.stat.biome(trans_table_eco.df$perc_trans_2C),
                           "PA_2C"=trans.stat.biome(trans_table_eco.df$perc_trans_2C_PA),
                           "tot_4C"=trans.stat.biome(trans_table_eco.df$perc_trans_4C),
                           "PA_4C"=trans.stat.biome(trans_table_eco.df$perc_trans_4C_PA))
write.csv(trans.biome.df, "Tables/transStats.csv", row.names=F)

