## confident interval  ###
Conf.interval=function(para.hat, var.hat, alpha)
{
  quan=qnorm(1-alpha/2)
  SE.hat=sqrt(var.hat)  
  
  ci.left =para.hat-quan*(SE.hat)
  ci.right=para.hat+quan*(SE.hat)
  
  return(list(ci.left=ci.left, ci.right=ci.right))
}