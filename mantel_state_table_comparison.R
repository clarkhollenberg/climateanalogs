# statistical tests for comparing state tables between global and PA
# 3/16/20
# Clark Hollenberg

# load table
setwd("~/Insync/clark.hollenberg@gmail.com/Google Drive/Analogs")

library(ade4)
statedatabio<-read.csv("Tables/state_table_biome.csv")
statedatabioSub<-statedatabio[-c(13),]  #remove no analogs
statedataeco<-read.csv("Tables/state_table_eco.csv")
statedataeco<-statedataeco[-c(849),] #remove insuff.data
statedataecoSub<-statedataeco[-c(848),] #remove no analogs
# basic mantel calc function
stateMantelCalc<-function(col1, col2)
{
  temp<-dist(col1)
  tempPA<-dist(col2)
  return(mantel.rtest(temp, tempPA, nrepet=999))
}

# create matrix with pairwise mantel tests of columns in stateData
genStateMantelMatrix<-function(statedata, colInd)
{
  out <- matrix(NA, nrow=length(colnames(statedata)[colInd]), ncol=length(colnames(statedata)[colInd]))
  out<-as.data.frame(out)
  rownames(out)<-colnames(statedata)[colInd]
  colnames(out)<-rownames(out)
  for (i in colInd[-length(colInd)])
  {
    print(i)
    for (k in (i+1):colInd[length(colInd)])
    {
      ni<-colnames(statedata)[i]
      nk<-colnames(statedata)[k]
      t<-stateMantelCalc(statedata[ni], statedata[nk])
      out[ni, nk]<-paste("r=",t$obs, "p=", t$pvalue)
    }
  }
  return(out)
}

mantelCalcsBio<-genStateMantelMatrix(statedatabio, colInd = 3:8)
mantelCalcsBioSub<-genStateMantelMatrix(statedatabioSub, colInd = 3:8)
mantelCalcsEco<-genStateMantelMatrix(statedataeco, colInd = 4:9)
mantelCalcsEcoSub<-genStateMantelMatrix(statedataecoSub, colInd = 4:9)

write.csv(mantelCalcsBio, "Tables/mantelCalcBio.csv")
write.csv(mantelCalcsBioSub, "Tables/mantelCalcBioSub.csv")
write.csv(mantelCalcsEco, "Tables/mantelCalcEco.csv")
write.csv(mantelCalcsEcoSub, "Tables/mantelCalcEcoSub.csv")
