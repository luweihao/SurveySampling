ratio=function(y.sample, x.sample, N=NULL, auxiliary=NULL, alpha)
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
  
  cy2=nf*sy2/(ybar^2)
  cx2=nf*sx2/(xbar^2)
  cyx=nf*syx/(ybar*xbar)
  
  ratio.est=ybar/xbar
  if(is.null(auxiliary)==TRUE)
  {
    ratio.var=(nf/xbar^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
  }
  else
  {
    ratio.var=(nf/auxiliary^2)*(sy2+ratio.est^2*sx2-2*ratio.est*syx)
  }

  ci1=Conf.interval(ratio.est, ratio.var, alpha)
  left1 =ci1$left
  right1=ci1$right
  
  quan = qnorm(1 - alpha/2)
  est2=1-quan^2*cyx
  var2=(cy2+cx2-2*cyx)-quan^2*(cy2*cx2-cyx^2)
  ci2=Conf.interval(est2, var2, alpha)
  left2 =ratio.est*(ci2$left)/(1-quan^2*cx2)
  right2=ratio.est*(ci2$right)/(1-quan^2*cx2)
  
  var3=ratio.est^2*(cy2+cx2-2*cyx)
  ci3=Conf.interval(ratio.est, var3, alpha)
  left3 =ci3$left
  right3=ci3$right
  
  ratio.ci=t(cbind(c(left1, right1), c(left2, right2), c(left3, right3)))
  colnames(ratio.ci)=c("left", "right")
  rownames(ratio.ci)=c("Classic", "Exact", "Exact2")
  
  return(list(ratio.est=ratio.est, ratio.var=ratio.var, ratio.ci=as.data.frame(ratio.ci)))
}