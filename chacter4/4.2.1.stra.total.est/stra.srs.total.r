##

source("Conf.interval.r")

stra.srs.total1=function(Nh, nh, yh, S2h, alpha)
{
  stra.num=length(Nh)
  fh=nh/Nh
  
  total.est.stra=rep(0, stra.num)
  total.var.stra=rep(0, stra.num)
  ci.left.stra =rep(0, stra.num)
  ci.right.stra=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    total.est.stra[h]=Nh[h]*yh[h]
    total.var.stra[h]=Nh[h]^2*((1-fh[h])/nh[h])*S2h[h]
    
    ci.stra=Conf.interval(total.est.stra[h], total.var.stra[h], alpha)
    ci.left.stra[h] =ci.stra$left
    ci.right.stra[h]=ci.stra$right
  }
  
  total.est=sum(total.est.stra)
  total.var=sum(total.var.stra)
  
  ci.result=Conf.interval(total.est, total.var, alpha)
  ci.left =ci.result$left
  ci.right=ci.result$right
  
  stra.total=cbind(total.est.stra, total.var.stra, ci.left.stra, ci.right.stra)
  total=matrix(c(total.est, total.var, ci.left, ci.right), nrow = 1)
  colnames(total)=c("total.est", "total.var", "ci.left", "ci.right")
  rownames(total)="Total"
  return(list(stra.total=as.data.frame(stra.total), total=as.data.frame(total)))
}
