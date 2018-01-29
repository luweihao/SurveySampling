clus.samesize=function(N, n, M, yi, alpha=0.05)
{
  y.doublebar.mean=sum(yi/M)/n
  y.total=N*M*y.doublebar.mean
  
  f=n/N
  y.doublebar.mean.var=(1-f)*var(yi/M)/n
  y.total.var=N^2*M^2*y.doublebar.mean.var
  
  ci=Conf.interval(y.doublebar.mean, y.doublebar.mean.var, alpha)
  left =ci$left
  right=ci$right
  
  clus.samesize=matrix(c(y.doublebar.mean, y.doublebar.mean.var, y.total, y.total.var, left, right), nrow = 1)
  colnames(clus.samesize)=c("mean.est", "mean.var", "total.est", "total.var", "left", "right")
  rownames(clus.samesize)="cluster_samesize"
  return(list(clus.samesize=as.data.frame(clus.samesize)))
}


clus.diffsize1=function(N, n, yi)
{
  yc.mean=sum(yi)/n
  y.total=N*yc.mean
  
  f=n/N
  yc.mean.var=(1-f)*var(yi)/n
  y.total.var=N^2*yc.mean.var
  
  clus.diffsize=matrix(c(yc.mean, yc.mean.var, y.total, y.total.var), nrow = 1)
  colnames(clus.diffsize)=c("mean.est", "mean.var", "total.est", "total.var")
  rownames(clus.diffsize)="cluster_M0unknown"
  return(list(clus.diffsize=as.data.frame(clus.diffsize)))
}


clus.diffsize2=function(N, n, yi, mi, M0)
{
  ys.mean=sum(yi)/sum(mi)
  y.total=M0*ys.mean
  
  f=n/N
  y.total.var=N^2*(1-f)*(sum(yi^2)+ys.mean^2*sum(mi^2)-2*ys.mean*sum(mi*yi))/(n*(n-1))
  ys.mean.var=y.total.var/M0^2
  
  clus.diffsize=matrix(c(ys.mean, ys.mean.var, y.total, y.total.var), nrow = 1)
  colnames(clus.diffsize)=c("mean.est", "mean.var", "total.est", "total.var")
  rownames(clus.diffsize)="cluster_M0known"
  return(list(clus.diffsize=as.data.frame(clus.diffsize)))
}