ratio.mean=function(y.sample, x.sample, n0, auxiliary)
{
  n=length(y.sample)
  
  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2=var(y.sample)
  sx2=var(x.sample)
  syx=cov(y.sample, x.sample)
  
  ratio.est=ybar/xbar
  rmean.est=ratio.est*auxiliary
  rmean.var=sy2/n+(1/n-1/n0)*(ratio.est^2*sx2-2*ratio.est*syx)
  
  return(list(ratio.mean.est=rmean.est, ratio.mean.var=rmean.var))
}