strata.total.Tsize=function(Nh, S2h, Ch=NULL, method, para, Ybar=NULL, alpha=NULL, allocation)
{
  N=sum(Nh)
  Wh=Nh/N
  
  if (method=="V")
  {
    if (allocation=="Prop")
    {
      wh=Wh
      n=sum(Wh^2*S2h/wh)/(para/N^2+sum(Wh*S2h)/N)
    }
    if (allocation=="Opt")
    {
      wh=(Wh*sqrt(S2h)/sqrt(Ch))/sum(Wh*sqrt(S2h)/sqrt(Ch))
      n=sum(Wh^2*S2h/wh)/(para/N^2+sum(Wh*S2h)/N)
    }
    if (allocation=="Neyman")
    {
      wh=(Wh*sqrt(S2h))/sum(Wh*sqrt(S2h))
      n=sum(Wh^2*S2h/wh)/(para/N^2+sum(Wh*S2h)/N)
    }
  }
  
  if (method=="CV")
  {
    if (allocation=="Prop")
    {
      wh=Wh
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/N^2+sum(Wh*S2h)/N)
    }
    if (allocation=="Opt")
    {
      wh=(Wh*sqrt(S2h)/sqrt(Ch))/sum(Wh*sqrt(S2h)/sqrt(Ch))
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/N^2+sum(Wh*S2h)/N)
    }
    if (allocation=="Neyman")
    {
      wh=(Wh*sqrt(S2h))/sum(Wh*sqrt(S2h))
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/N^2+sum(Wh*S2h)/N)
    }
  }
  
  
  if (method=="d")
  {
    quan=qnorm(1-alpha/2)
    
    if (allocation=="Prop")
    {
      wh=Wh
      n=sum(Wh^2*S2h/wh)/(para^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
    if (allocation=="Opt")
    {
      wh=(Wh*sqrt(S2h)/sqrt(Ch))/sum(Wh*sqrt(S2h)/sqrt(Ch))
      n=sum(Wh^2*S2h/wh)/(para^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
    if (allocation=="Neyman")
    {
      wh=(Wh*sqrt(S2h))/sum(Wh*sqrt(S2h))
      n=sum(Wh^2*S2h/wh)/(para^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
  }
  
  if (method=="r")
  {
    quan=qnorm(1-alpha/2)
    
    if (allocation=="Prop")
    {
      wh=Wh
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
    if (allocation=="Opt")
    {
      wh=(Wh*sqrt(S2h)/sqrt(Ch))/sum(Wh*sqrt(S2h)/sqrt(Ch))
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
    if (allocation=="Neyman")
    {
      wh=(Wh*sqrt(S2h))/sum(Wh*sqrt(S2h))
      n=sum(Wh^2*S2h/wh)/(para^2*Ybar^2/(quan^2*N^2)+sum(Wh*S2h)/N)
    }
  }
  
  return(list(method=method, para=para, allocation=allocation, n=round(n)))
}