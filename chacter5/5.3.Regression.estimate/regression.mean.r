regression.mean=function(y.sample, x.sample, N=NULL, Xbar, alpha)
{
  n=length(y.sample)
  
  if(is.null(N)==TRUE)
  {
    f=0
    nf=(1-f)/n
  }
  else
  {
    f=n/N
    nf=(1-f)/n
  }
  
  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2=var(y.sample)
  sx2=var(x.sample)
  syx=cov(y.sample, x.sample)
  
  beta.ols=syx/sx2
  rho=syx/sqrt(sy2*sx2)
  
  reg.mean.est=ybar+beta.ols*(Xbar-xbar)
  reg.mean.var=nf*sy2*(1-rho^2)
  
  ci1=Conf.interval(reg.mean.est, reg.mean.var, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  reg.ci=matrix(c(left1, right1), nrow = 1)
  colnames(reg.ci)=c("left", "right")
  rownames(reg.ci)=c("Classic")
  
  return(list(reg.mean.est=reg.mean.est, reg.mean.var=reg.mean.var, reg.mean.ci=as.data.frame(reg.ci)))
}