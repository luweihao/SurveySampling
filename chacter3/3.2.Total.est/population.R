population=function(N, mysample, alpha=0.05)
{
  n=length(mysample)
  f=n/N
  
  ymean=mean(mysample)
  Y_total=ymean*N
  
  ys2  =var(mysample)
  yvar =(1-f)/n*ys2
  Y_var=yvar*N^2
  
  yci =Conf.interval(Y_total, Y_var, alpha)
  
  yleft =yci$ci.left
  yright=yci$ci.right
  ci=paste("[",yleft,",",yright,"]")
  
  return(list(total.est=Y_total, var.est=Y_var, ci=ci))
}