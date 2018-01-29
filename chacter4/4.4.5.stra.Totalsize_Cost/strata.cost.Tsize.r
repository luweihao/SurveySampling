strata.cost.Tsize=function(Nh, S2h, Ctotal, Cfixed, Ch, allocation)
{
  N=sum(Nh)
  Wh=Nh/N
  
  n=(Ctotal-Cfixed)*sum(Wh*S2h/sqrt(Ch))/sum(Wh*S2h*sqrt(Ch))
  
  if (allocation=="Prop"){nh=n*Nh/sum(Nh)}
  if (allocation=="Opt"){nh=n*(Nh*sqrt(S2h)/sqrt(Ch))/sum(Nh*sqrt(S2h)/sqrt(Ch))}
  if (allocation=="Neyman"){nh=n*(Nh*sqrt(S2h))/sum(Nh*sqrt(S2h))}
  
  return(list(n=round(n), allocation=allocation, nh=round(nh)))
}
