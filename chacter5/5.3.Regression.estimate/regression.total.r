regression.total=function(y.sample, x.sample, N, Xbar, alpha)
{
  n=length(y.sample)
  f=n/N
  nf=(1-f)/n
  
  ybar=mean(y.sample)
  xbar=mean(x.sample)
  
  sy2=var(y.sample)
  sx2=var(x.sample)
  syx=cov(y.sample, x.sample)
  
  beta.ols=syx/sx2
  rho=syx/sqrt(sy2*sx2)
  
  reg.total.est=N*(ybar+beta.ols*(Xbar-xbar))
  reg.total.var=N^2*nf*sy2*(1-rho^2)
  
  ci1=Conf.interval(reg.total.est, reg.total.var, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  reg.ci=matrix(c(left1, right1), nrow = 1)
  colnames(reg.ci)=c("left", "right")
  rownames(reg.ci)=c("Classic")
  
  return(list(reg.total.est=reg.total.est, reg.total.var=reg.total.var, reg.total.ci=as.data.frame(reg.ci)))
}