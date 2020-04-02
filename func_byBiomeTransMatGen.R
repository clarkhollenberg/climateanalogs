#build list containing matrix for each biome
generateByBiomeList <- function(data_eco, data_bio, index)
{
  for (i in index)
  {
    if (i==9)
    {
      #create another dataframe to hold water biome spot (just to not mess up indexing by biomeNum)
      biomeIndex<-genBiomeIndex(8, data_bio)
      list<-append(list, list(cleanTransMatByBiome(data_eco, 8, biomeIndex)))
    }
    else
    {
      biomeIndex<-genBiomeIndex(i, data_bio)
      if (i==1)
      {
        list<-list(cleanTransMatByBiome(data_eco, i, biomeIndex))
      }
      else
      {
        list<-append(list, list(cleanTransMatByBiome(data_eco, i, biomeIndex)))
      }
    }
  }
  return(list)
}

#creates name index of biomes that contribute to the focal biome
genBiomeIndex <- function(biomeNum, data_bio)
{
  biomeName<-as.character(subset(LUT_biome, BIOME_ID==biomeNum)$BIOME_NAME)
  
  #first determine which external biomes are contributing and should be additional rows/columns
  rows=data_bio[biomeName, ]>0
  cols=data_bio[, biomeName]>0
  #if rows & cols both are 0, then that biome can be excluded.
  biome_incl=rows+cols  #exclude if ==0
  biome_incl[, biomeName]<-0  #get rid of irrelevant biomes and current biome
  data_bio[, biome_incl==0]<-NULL
  data_bio<-data_bio[colnames(data_bio), ]
  return(colnames(data_bio))
}

##create matrix subset for that biome including all other biomes contributions to its ecoregions
cleanTransMatByBiome<-function(data_eco, biomeNum, biomeIndex)
{
  #take the subset of full eco matrix with biome of interest - we will add on biome columns/rows to this
  data_eco_sub<-data_eco[, colnames(data_eco) %in% subset(LUT_plus, BIOME_ID==biomeNum)$econame]
  data_eco_sub<-data_eco_sub[colnames(data_eco_sub), ]
  
  for (i in biomeIndex)
  {
    #take the columns in each external biome and sum their contributions within the original biome
    bio_col<-data_eco[, colnames(data_eco) %in% subset(LUT_plus, BIOME_NAME==i)$econame] %>% as.data.frame()
    rownames(bio_col)<-rownames(data_eco)
    bio_col<-subset(bio_col, rownames(bio_col) %in% rownames(data_eco_sub)) %>% rowSums()
    data_eco_sub<-cbind(data_eco_sub, bio_col)
    #label the column we just added with correct biome name
    colnames(data_eco_sub)[length(colnames(data_eco_sub))]<-i
  }
  for (i in biomeIndex)
  {
    #take the rows in each external biome and sum their contributions within the original biome
    bio_row<-data_eco[rownames(data_eco) %in% subset(LUT_plus, BIOME_NAME==i)$econame, ] %>%as.data.frame()
    colnames(bio_row)<-colnames(data_eco)
    #take only columns in original biome
    bio_row<-bio_row[, colnames(bio_row) %in% subset(LUT_plus, BIOME_ID==biomeNum)$econame] %>% colSums() %>% append(., rep(0, times=length(biomeIndex)))
    data_eco_sub<-rbind(data_eco_sub, bio_row)
    #label the row we just added with correct biome name
    rownames(data_eco_sub)[length(rownames(data_eco_sub))]<-i
  }
  return(data_eco_sub)
}