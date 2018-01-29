stra.sampling=function(Nh, nh, full.data)
{
  stra.num=length(Nh)
  
  data.1th=full.data[full.data$stra==1,]
  subset.1th=sample(1:Nh[1], nh[1])
  sample.1th=data.1th[subset.1th,]
  stra.sample=sample.1th
  
  for (h in 2:stra.num)
  {
    data.hth=full.data[full.data$stra==h,]
    subset.hth=sample(1:Nh[h], nh[h])
    sample.hth=data.hth[subset.hth,]
    
    stra.sample=rbind(stra.sample,sample.hth)
  }
  return(stra.sample)
}