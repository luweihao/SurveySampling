## 4.4.3 样本总量确定——估计总体总量情形

source("strata.size.r")
source("strata.total.Tsize.r")

Nh=c(112, 68, 39)
S2h=c(2.25, 3.24, 3.24)
Ch=c(9, 25, 36)
method="V"
para=0.1*sum(Nh)^2
allocation="Opt"

Tre4.4.3=strata.total.Tsize(Nh, S2h, Ch, method, para, Ybar=NULL, alpha=NULL, allocation)
re4.4.3=strata.size(Tre4.4.3$n, Nh, S2h, Ch, allocation)
print(Tre4.4.3)
print(re4.4.3)