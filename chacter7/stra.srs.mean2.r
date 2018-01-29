##

source("Conf.interval.r")

stra.srs.mean2=function(Nh, nh, mydata, stra, alpha, N=NULL)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  
  mean.est.stra=rep(0, stra.num)
  mean.var.stra=rep(0, stra.num)
  ci.left.stra =rep(0, stra.num)
  ci.right.stra=rep(0, stra.num)
  yh =rep(0, stra.num)
  S2h=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    yh[h]=mean(mydata[stra==h])
    S2h[h]=var(mydata[stra==h])
    mean.est.stra[h]=yh[h]
    mean.var.stra[h]=S2h[h]
    
    ci.stra=Conf.interval(mean.est.stra[h], mean.var.stra[h], alpha)
    ci.left.stra[h] =ci.stra$left
    ci.right.stra[h]=ci.stra$right
  }
  
  mean.est=sum(Wh*mean.est.stra)
  if(is.null(N)==FALSE){
    mean.var=sum((1/nh-1/Nh)*Wh^2*mean.var.stra)+(1/sum(Nh)-1/N)*sum(Wh*(yh-mean.est)^2)
  }else{
    mean.var=sum((1/nh-1/Nh)*Wh^2*mean.var.stra)+(1/sum(Nh))*sum(Wh*(yh-mean.est)^2)
  }
  
  
  ci.result=Conf.interval(mean.est, mean.var, alpha)
  ci.left =ci.result$left
  ci.right=ci.result$right
  
  stra.mean=cbind(mean.est.stra, mean.var.stra, ci.left.stra, ci.right.stra)
  total.mean=matrix(c(mean.est, mean.var, ci.left, ci.right), nrow = 1)
  colnames(total.mean)=c("mean.est", "mean.var", "ci.left", "ci.right")
  rownames(total.mean)="Total"
  return(list(stra.mean=as.data.frame(stra.mean), total.mean=as.data.frame(total.mean)))
}
