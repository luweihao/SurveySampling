choosen_givenpro=function(p = 0.5, quan, para, x)
{
  q <- 1-p
  if (x==1){
    v.max  <- para  # Maximal variance
    n0 <- p*q/v.max
  }
  if (x==2){
    cv.max <- para  # Coefficient variance
    n0 <- q/(p*cv.max^2)
  }
  if (x==3){
    d.max  <- para  # Absolute error limit
    n0 <- quan^2*p*q/d.max^2
  }
  if (x==4){
    r.max  <- para  # Relative error limit
    n0 <- quan^2*q/(p*r.max^2)
  }
  return(ceiling(n0))
}