stra.sep.ratio.mean=function(ly, ty, stra, Nh, nh, Xbar)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  nf=(1-fh)/nh
  
  ratio.est=rep(0, stra.num)
  rmean.est=rep(0, stra.num)
  rmean.var=rep(0, stra.num)
  ybar=rep(0, stra.num)
  xbar=rep(0, stra.num)
  sy2=rep(0, stra.num)
  sx2=rep(0, stra.num)
  syx=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    ybar[h]=mean(ty[stra==h])
    xbar[h]=mean(ly[stra==h])
    
    sy2[h]=var(ty[stra==h])
    sx2[h]=var(ly[stra==h])
    syx[h]=cov(ty[stra==h], ly[stra==h])
    
    ratio.est[h]=ybar[h]/xbar[h]
    rmean.est[h]=ratio.est[h]*Xbar[h]
    rmean.var[h]=nf[h]*(sy2[h]+ratio.est[h]^2*sx2[h]-2*ratio.est[h]*syx[h])
  }
  
  SSR.mean.est=sum(Wh*rmean.est)
  SSR.mean.var=sum(Wh^2*rmean.var)
  
  SSR.mean=cbind(rmean.est, rmean.var)
  sep.ratio.mean=matrix(c(SSR.mean.est, SSR.mean.var), nrow = 1)
  colnames(sep.ratio.mean)=c("mean.est", "mean.var")
  rownames(sep.ratio.mean)="separate.ratio"
  return(list(SSR.mean=as.data.frame(SSR.mean), sep.ratio.mean=as.data.frame(sep.ratio.mean)))
}

stra.com.ratio.mean=function(ly, ty, stra, Nh, nh, Xbar)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  nf=(1-fh)/nh
  ratio.est=mean(ty)/mean(ly)
  
  rmean.var=rep(0, stra.num)
  ybar=rep(0, stra.num)
  xbar=rep(0, stra.num)
  sy2=rep(0, stra.num)
  sx2=rep(0, stra.num)
  syx=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    ybar[h]=mean(ty[stra==h])
    xbar[h]=mean(ly[stra==h])
    
    sy2[h]=var(ty[stra==h])
    sx2[h]=var(ly[stra==h])
    syx[h]=cov(ty[stra==h], ly[stra==h])
    
    rmean.var[h]=nf[h]*(sy2[h]+ratio.est^2*sx2[h]-2*ratio.est*syx[h])
  }
  
  SCR.mean.est=sum(Wh*ybar)*sum(Wh*Xbar)/sum(Wh*xbar)
  SCR.mean.var=sum(Wh^2*rmean.var)
  
  com.ratio.mean=matrix(c(SCR.mean.est, SCR.mean.var), nrow = 1)
  colnames(com.ratio.mean)=c("mean.est", "mean.var")
  rownames(com.ratio.mean)="combined.ratio"
  return(list(com.ratio.mean=as.data.frame(com.ratio.mean)))
}