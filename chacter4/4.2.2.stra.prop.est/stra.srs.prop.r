##

source("Conf.interval.r")

stra.srs.prop1=function(Nh, nh, ah, alpha)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  
  prop.est.stra=rep(0, stra.num)
  prop.var.stra=rep(0, stra.num)
  ci.left.stra =rep(0, stra.num)
  ci.right.stra=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    prop.est.stra[h]=ah[h]/nh[h]
    prop.var.stra[h]=((1-fh[h])/(nh[h]-1))*prop.est.stra[h]*(1-prop.est.stra[h])
    
    ci.stra=Conf.interval(prop.est.stra[h], prop.var.stra[h], alpha)
    ci.left.stra[h] =ci.stra$left
    ci.right.stra[h]=ci.stra$right
  }
  
  prop.est=sum(Wh*prop.est.stra)
  prop.var=sum(Wh^2*prop.var.stra)
  
  ci.result=Conf.interval(prop.est, prop.var, alpha)
  ci.left =ci.result$left
  ci.right=ci.result$right
  
  stra.prop=cbind(prop.est.stra, prop.var.stra, ci.left.stra, ci.right.stra)
  total.prop=matrix(c(prop.est, prop.var, ci.left, ci.right), nrow = 1)
  colnames(total.prop)=c("prop.est", "prop.var", "ci.left", "ci.right")
  rownames(total.prop)="Total"
  return(list(stra.prop=as.data.frame(stra.prop), total.prop=as.data.frame(total.prop)))
}
