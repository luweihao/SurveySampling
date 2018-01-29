mean.srs=function(N, mysample, alpha=0.05)
{
  n=length(mysample)
  f=n/N
  
  ymean=mean(mysample)
  ys2  =var(mysample)
  
  yvar=(1-f)/n*ys2
  yci =Conf.interval(ymean, yvar, alpha)
  
  yleft =yci$ci.left
  yright=yci$ci.right
  ci=paste("[",yleft,",",yright,"]")
  
  return(list(mean.est=ymean, var.est=yvar, ci=ci))
}