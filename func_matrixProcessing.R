`%notin%` <- Negate(`%in%`)

#for input flow matrices
#################################################################################
#remove values where row sums are 0 and map names to rows/columns
cleanInput<-function(data, reflexive =F, biome=F, row=F)
{
  #remove out fluxes that return to the same sector - ie set diagonal=0 (optional, default is F)
  if (reflexive==T)
  {
    diag(data)<-0
  }
  if (biome)
  {
    colnames(data)<-LUT_biome$BIOME_NAME[-17]  #take bionames minus the insuff.data
    rownames(data)<-colnames(data)
    data<-data[-c(9, 14, 15)]   #remove water biomes and r&i
    data<-data[-c(9, 14, 15), ]
  }
  else
  {
    colnames(data)<-LUT_plus$econame[-849]  #take econames minus the insuff.data
    rownames(data)<-colnames(data)
  }
  data[is.na(data)]<-0
  
  #to just remove where rows=0 (for the transition matrix this is useful - do not use for chord diagrams) --this could return an asymmetrical matrix
  if (row==T)
  {
    rows<-rowSums(data)
    #remove columns and rows with where ecoregion is not involved in giving or receiving area
    data<-data[(rows!=0), ]
  }
  else
  {
    cols<-colSums(data)
    rows<-rowSums(data)
    #remove columns and rows with where ecoregion is not involved in giving or receiving area
    data[, (cols==0 & rows==0)]<-NULL  
    data<-data[colnames(data), ]  #make the matrix symmetrical
  }
  
  return(data)
}

#change from adjacency matrix to adjacency list
pipeToLongFormat<-function(data)
{
  data_long <- data %>%
    rownames_to_column %>%
    gather(key = 'key', value = 'value', -rowname)
  data_long[is.na(data_long)]<-0
  return(data_long)
}

#remove rows with zero row sums and make symmetric
rmZeroRows<-function(data)
{
  rows<-rowSums(data)!=0
  data<-data[, rows]
  data<-data[colnames(data),]
  return(data)
}

#return index of columns to keep that are above a minumum percent of total area (for chords)
trimInd<-function(data, minPerc)
{
  totArea<-sum(colSums(data))
  temp<-abs((colSums(data)+rowSums(data))/totArea) > minPerc
  ind<-colnames(data)[temp]
  return(ind)
}

#divide values by row sums (transform to probability matrix)
transitionMatrix<-function(data)
{
  data[is.na(data)]<-0  #set any NAs to 0
  #divide by row sums
  rows<-rowSums(data)
  data_tr<-data / rows
  data_tr[rows==0,]<-NA
  data_tr[is.na(data_tr)]<-0
  # data_tr[is.nan(data_tr)]=NA  #when we divide by 0 (meaning that the initial total area is equal to 0) set this to NA, indicating that there is no initial area
  return(data_tr)
}

#change the row/col names of matrix to ECO/BIO_ID (from eco/bionames)
matNamesToNum<-function(data)
{
  colNm<-subset(LUT_plus, econame %in% colnames(data)[-length(colnames(data))])$ECO_ID
  colNm<-c(colNm, paste0("B", unique(subset(LUT_plus[order(as.numeric(LUT_plus$BIOME_ID)),], BIOME_NAME %in% colnames(data))$BIOME_ID)))
  colnames(data)<-colNm
  rownames(data)<-colnames(data)
  return(data)
}