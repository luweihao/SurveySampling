## 4.2.2.对总体比例的估计

source("stra.srs.prop.r")
source("stra.srs.prop2.r")

Nh=c(0.281, 0.322, 0.213, 0.184)*100000
nh=c(400, 650, 600, 350)
ph=c(0.083, 0.174, 0.310, 0.464)
alpha=0.05
re1=stra.srs.prop1(Nh, nh, ah=ph*nh, alpha)
print(re1)


Nh=c(100, 100, 100, 100)
nh=c(5, 5, 5, 5)
mydata=c(1, 0, 1, 1, 1,
         0, 0, 0, 1, 1,
         1, 1, 1, 1, 1,
         0, 1, 0, 1, 1)
stra    =c(1, 1, 1, 1, 1,
           2, 2, 2, 2, 2,
           3, 3, 3, 3, 3,
           4, 4, 4, 4, 4)
alpha=0.05

re2=stra.srs.prop2(Nh, nh, mydata, stra, alpha)
print(re2)