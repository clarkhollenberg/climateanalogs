#function to calculate bootstrap confidence interval from sampled data

mta_biomeBootstrap<-function(inData, column)
{
  for (i in c(1:8, 10:13))
  {
    df<-column[inData$BIOME_ID==i]
    if (i==1)
    {vec<-matrix(bootstrapCI(df, 0.95, nResamples = 1000), nrow=1, ncol=2)}
    else
    {
      vec<-rbind(vec, bootstrapCI(df, 0.95, nResamples = 1000))
    }
  }
  return(vec)
}

bootstrapCI<-function(data, ci, nResamples)
{
  data<-data[!is.na(data)]
  emean=mean(data)
  mat<-matrix(sample(data, size=nResamples*length(data), replace=T), nrow=length(data), ncol=nResamples)
  reMeans<-apply(mat, MARGIN=2, FUN=mean)
  reMeans<-reMeans-emean
  reMeans<-reMeans[order(reMeans)]
  
  #index based on ci
  ind<-round((1-ci)*nResamples)
  lower<-emean+reMeans[ind]
  upper<-emean+reMeans[nResamples-ind]
  return(c(lower, upper))
}

bootstrapCIbase<-function(data, ci=0.95, func=abs02, nResamples=5000)
{
  data<-data[!is.na(data)]
  eFun=func(data)
  mat<-matrix(sample(data, size=nResamples*length(data), replace=T), nrow=length(data), ncol=nResamples)
  reFun<-apply(mat, MARGIN=2, FUN=func)
  reFun<-reFun-eFun
  reFun<-reFun[order(reFun)]
  
  #index based on ci
  ind<-round((1-ci)*nResamples)
  upper<-reFun[nResamples-ind]
  return(c(upper))
}
