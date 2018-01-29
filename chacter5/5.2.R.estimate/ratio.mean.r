ratio.mean=function(y.sample, x.sample, N=NULL, auxiliary, alpha)
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
  
  ratio.est=ybar/xbar
  rmean.est=ratio.est*auxiliary
  rmean.var=nf*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
  
  ci1=Conf.interval(rmean.est, rmean.var, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  ratio.ci=matrix(c(left1, right1), nrow = 1)
  colnames(ratio.ci)=c("left", "right")
  rownames(ratio.ci)=c("Classic")
  
  deff=1+(ratio.est^2*sx2-2*ratio.est*syx)/sy2
  
  return(list(ratio.mean.est=rmean.est, ratio.mean.var=rmean.var, ratio.ci=as.data.frame(ratio.ci), deff=deff))
}