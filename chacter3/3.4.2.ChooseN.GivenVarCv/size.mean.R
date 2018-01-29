## 3.4.2估计总体均值时样本量的确定方法

##example3.7
source("choosen_given.r")
alpha <- 0.05  # Conf.Level=1-alpha
quan  <- qnorm(1-alpha/2)
N=NULL

var.est=4^2
##mean.est <- 10
##cv <- sqrt(var.est)/mean.est

##cv <- 0.38  # Already known

###   Optional: Choose one    ###
#(1)Maximal variance      (2)Coefficient variance
#(3)Absolute error limit  (4)Relative error limit
para.max <- 0.2  # Corresponding max value
x <- 3  # Option(1,3use var.est, 2,4use cv)

n.result <- choosen_given(var.est, quan, para.max, x)

if (is.null(N)==FALSE){
  n.result <- ceiling(n.result/(1 + n.result/N))
}

s <- paste("The suitable n is", n.result)
sapply(s, print, quote = FALSE)


#######
source("choosen_given.r")
alpha <- 0.05  # Conf.Level=1-alpha
quan  <- qnorm(1-alpha/2)
N=NaN

##var.est  <- 100
##mean.est <- 10
##cv <- sqrt(var.est)/mean.est

cv <- 0.38  # Already known

      ###   Optional: Choose one    ###
#(1)Maximal variance      (2)Coefficient variance
#(3)Absolute error limit  (4)Relative error limit
para.max <- 0.04  # Corresponding max value
x <- 4  # Option(1,3use var.est, 2,4use cv)

n.result <- choosen_given(cv, quan, para.max, x)

if (is.null(N)==FALSE){
  n.result <- ceiling(n.result/(1 + n.result/N))
}

s <- paste("The suitable n is", n.result)
sapply(s, print, quote = FALSE)