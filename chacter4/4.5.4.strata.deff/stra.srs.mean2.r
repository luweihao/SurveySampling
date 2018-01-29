stra.srs.mean2=function(Nh, mydata, stra)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  yh =rep(0, stra.num)
  S2h=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    yh[h]=mean(mydata[stra==h])
    S2h[h]=var(mydata[stra==h])
  }
  
  return(list(mean.stra=yh, S2h.stra=S2h))
}