stra.sep.regression.mean=function(ly, ty, stra, Nh, nh, Xbar)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  nf=(1-fh)/nh
  
  rmean.est=rep(0, stra.num)
  rmean.var=rep(0, stra.num)
  ybar=rep(0, stra.num)
  xbar=rep(0, stra.num)
  sy2=rep(0, stra.num)
  sx2=rep(0, stra.num)
  syx=rep(0, stra.num)
  beta.ols=rep(0, stra.num)
  rho=rep(0, stra.num)
  
  for(h in 1:stra.num)
  {
    ybar[h]=mean(ty[stra==h])
    xbar[h]=mean(ly[stra==h])
    
    sy2[h]=var(ty[stra==h])
    sx2[h]=var(ly[stra==h])
    syx[h]=cov(ty[stra==h], ly[stra==h])
    
    beta.ols[h]=syx[h]/sx2[h]
    rho[h]=syx[h]/sqrt(sy2[h]*sx2[h])
    
    rmean.est[h]=ybar[h]+beta.ols[h]*(Xbar[h]-xbar[h])
    rmean.var[h]=nf[h]*sy2[h]*(1-rho[h]^2)*(nh[h]-1)/(nh[h]-2)
  }
  
  SSR.mean.est=sum(Wh*rmean.est)
  SSR.mean.var=sum(Wh^2*rmean.var)
  
  SSR.mean=cbind(rmean.est, rmean.var)
  sep.reg.mean=matrix(c(SSR.mean.est, SSR.mean.var), nrow = 1)
  colnames(sep.reg.mean)=c("mean.est", "mean.var")
  rownames(sep.reg.mean)="separate.regression"
  return(list(SSR.mean=as.data.frame(SSR.mean), sep.reg.mean=as.data.frame(sep.reg.mean)))
}

stra.com.regression.mean=function(ly, ty, stra, Nh, nh, Xbar)
{
  stra.num=length(Nh)
  Wh=Nh/sum(Nh)
  fh=nh/Nh
  nf=(1-fh)/nh
  
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
  }
  
  beta.ols=sum(Wh^2*nf*syx)/sum(Wh^2*nf*sx2)
  SCR.mean.est=sum(Wh*ybar)+beta.ols*(sum(Wh*Xbar)-sum(Wh*xbar))
  SCR.mean.var=sum(Wh^2*nf*(sy2-2*beta.ols*syx+beta.ols^2*sx2))
  
  com.reg.mean=matrix(c(SCR.mean.est, SCR.mean.var), nrow = 1)
  colnames(com.reg.mean)=c("mean.est", "mean.var")
  rownames(com.reg.mean)="combined.regression"
  return(list(com.reg.mean=as.data.frame(com.reg.mean)))
}