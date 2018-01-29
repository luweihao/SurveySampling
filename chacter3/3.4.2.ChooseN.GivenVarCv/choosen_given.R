choosen_given=function(given, quan, para, x)
{
  if (x==1){
    v.max  <- para  # Maximal variance
    n0 <- given/v.max
  }
  if (x==2){
    cv.max <- para  # Coefficient variance
    n0 <- given^2/cv.max^2
  }
  if (x==3){
    d.max  <- para  # Absolute error limit
    n0 <- quan^2*given/d.max^2
  }
  if (x==4){
    r.max  <- para  # Relative error limit
    n0 <- (quan*given)^2/r.max^2
  }
  return(ceiling(n0))
}