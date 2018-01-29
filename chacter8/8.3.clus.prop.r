#8.3.2估计总体比例的整群抽样，群大小相等:example8.5
source("clus.r")
source("conf_interval.R")

N=512
n=12
M=8
ai=c(4, 3, 5, 6, 3, 4,
     5, 2, 6, 4, 3, 5)
##ai=M*pi
alpha=0.05

re1=clus.samesize(N=N, n=n, M=M, yi=ai, alpha=alpha)
print(re1$clus.samesize$mean.est)
print(re1$clus.samesize$mean.var)


#8.3.3估计总体比例的整群抽样，群大小不等:example8.6
clus.diffsize2=function(n, yi, mi, mbar, N=NULL)
{
  ys.mean=sum(yi)/sum(mi)
  
  if(is.null(N)){
    ys.mean.var=(sum(yi^2)+ys.mean^2*sum(mi^2)-2*ys.mean*sum(mi*yi))/(n*(n-1)*mbar^2)
  }else{
    f=n/N
    ys.mean.var=(1-f)*(sum(yi^2)+ys.mean^2*sum(mi^2)-2*ys.mean*sum(mi*yi))/(n*(n-1)*mbar^2)
  }
  
  clus.diffsize=matrix(c(ys.mean, ys.mean.var), nrow = 1)
  colnames(clus.diffsize)=c("mean.est", "mean.var")
  rownames(clus.diffsize)="cluster_M0known"
  return(list(clus.diffsize=as.data.frame(clus.diffsize)))
}

N=NULL
n=56
ai=c(1,1,3,3,1,1,3,1,2,1,
     2,3,2,4,0,1,3,2,1,1,
     2,2,1,2,4,2,2,2,1,2,
     1,2,2,1,3,1,2,2,3,2,
     1,2,2,3,0,0,2,1,1,3,
     2,1,2,1,3,2)
bi=c(3,2,1,2,2,1,2,3,1,1,
     1,1,1,2,1,1,3,2,2,1,
     2,0,0,1,2,2,2,3,2,2,
     1,0,2,2,4,1,3,2,3,0,
     2,2,1,2,2,3,1,1,2,2,
     2,1,1,2,2,2)

mi=ai+bi
mbar=sum(mi)/n

re2=clus.diffsize2(n=n, yi=ai, mi=mi, mbar=mbar, N)
print(re2$clus.diffsize$mean.est)
print(re2$clus.diffsize$mean.var)

re3=clus.diffsize2(n=n, yi=bi, mi=mi, mbar=mbar, N)
print(re3$clus.diffsize$mean.est)
print(re3$clus.diffsize$mean.var)

