deff=function(var.result)
{
  deff.vector=var.result/var.result[1]
  deff.result=rbind(var.result, deff.vector)
  rownames(deff.result)=c("Var", "Deff")
  deff.result=round(deff.result, 4)
  return(deff.result)
}