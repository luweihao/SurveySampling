## 3.4.2�����������ʱ��������ȷ������:��3.8

source("choosen_givenpro.r")

#######
alpha <- 0.05  # Conf.Level=1-alpha
quan <- qnorm(1-alpha/2)
N=NULL

p <- 0.018  # default=0.5

      ###   Optional: Choose one    ###
#(1)Maximal variance      (2)Coefficient variance
#(3)Absolute error limit  (4)Relative error limit
para <- 0.05  # Corresponding max value
x <- 4 # Option
n.result <- choosen_givenpro(p, quan, para, x)

if (is.null(N)==FALSE){
  n.result <- ceiling(n.result/(1 + n.result/N))
}

print("The suitable n is")
print(n.result)