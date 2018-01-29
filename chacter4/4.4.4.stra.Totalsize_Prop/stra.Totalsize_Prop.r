## 4.4.4 样本总量确定——估计总体比例情形

source("strata.size.r")
source("strata.prop.Tsize.r")

Nh=c(0.281, 0.322, 0.213, 0.184)*100000
Ph=c(0.083, 0.174, 0.310, 0.464)
method="r"
para=0.05

allocation="Prop"
Tre4.4.4=strata.prop.Tsize(Nh, Ph, Ch=NULL, method, para, P0bar=0.25, alpha=0.05, allocation)
re4.4.4=strata.size(Tre4.4.4$n, Nh, S2h=Ph*(1-Ph), Ch=NULL, allocation)
print(Tre4.4.4)
print(re4.4.4)

allocation="Neyman"
Tre4.4.4=strata.prop.Tsize(Nh, Ph, Ch=NULL, method, para, P0bar=0.25, alpha=0.05, allocation)
re4.4.4=strata.size(Tre4.4.4$n, Nh, S2h=Ph*(1-Ph), Ch=NULL, allocation)
print(Tre4.4.4)
print(re4.4.4)