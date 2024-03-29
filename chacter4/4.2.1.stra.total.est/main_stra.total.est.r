## 4.2.1.对总体总量的估计

source("stra.srs.total.r")
source("stra.srs.total2.r")

Nh=c(23560, 148420)
nh=c(300, 250)
yh=c(15180, 9856)
S2h=c(3972^2, 2546^2)
alpha=0.1
re1=stra.srs.total1(Nh, nh, yh, S2h, alpha)
print(re1)


Nh=c(86, 72, 52, 30)
nh=c(14, 12, 9, 5)
mydata=c(97, 67, 42, 125, 25, 92, 105, 86, 27, 43, 45, 59, 53, 52,
         125, 155, 67, 96, 256, 47, 310, 236, 220, 352, 142, 190,
         142, 256, 310, 440, 495, 510, 320, 396, 196,
         167, 655, 220, 540, 780)
stra    =c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
           3, 3, 3, 3, 3, 3, 3, 3, 3,
           4, 4, 4, 4, 4)
alpha=0.05

re2=stra.srs.total2(Nh, nh, mydata, stra, alpha)
print(re2)