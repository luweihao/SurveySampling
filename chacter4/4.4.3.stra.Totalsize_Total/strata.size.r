strata.size=function(n, Nh, S2h, Ch=NULL, allocation)
{
  if (allocation=="Prop"){nh=n*Nh/sum(Nh)}
  if (allocation=="Opt"){nh=n*(Nh*sqrt(S2h)/sqrt(Ch))/sum(Nh*sqrt(S2h)/sqrt(Ch))}
  if (allocation=="Neyman"){nh=n*(Nh*sqrt(S2h))/sum(Nh*sqrt(S2h))}
  
  return(list(nh=round(nh)))
}
