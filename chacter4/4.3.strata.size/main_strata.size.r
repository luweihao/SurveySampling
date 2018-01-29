## 4.3 各层样本量的分配

source("strata.size.r")

n=27
Nh=c(112, 68, 39)
S2h=c(2.25, 3.24, 3.24)
Ch=c(9, 25, 36)
allocation="Opt"

re4.3=strata.size(n, Nh, S2h, Ch, allocation)
print(re4.3)