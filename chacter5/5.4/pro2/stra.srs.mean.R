stra.srs.mean=function(Nh, nh, mydata, stra)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  
  mean.est.stra=rep(0, stra.num)
  mean.var.stra=rep(0, stra.num)
  yh =rep(0, stra.num)
  S2h=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    yh[h]=mean(mydata[stra==h])
    S2h[h]=var(mydata[stra==h])
    mean.est.stra[h]=yh[h]
    mean.var.stra[h]=((1-fh[h])/nh[h])*S2h[h]
  }
  
  mean.est=sum(Wh*mean.est.stra)
  mean.var=sum(Wh^2*mean.var.stra)
  
  stra.mean=cbind(mean.est.stra, mean.var.stra)
  total.mean=matrix(c(mean.est, mean.var), nrow = 1)
  colnames(total.mean)=c("mean.est", "mean.var")
  rownames(total.mean)="Total"
  return(list(stra.mean=as.data.frame(stra.mean), total.mean=as.data.frame(total.mean)))
}