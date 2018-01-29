## 4.5.4.从分层样本估计分层抽样的结果

source("stra.srs.mean2.r")


Nh=c(0.54164, 0.30996, 0.03025, 0.11815)*100000
N=sum(Nh)
nh=c(300, 200, 50, 50)
n=sum(nh)
yh=c(25.5, 35.4, 48.5, 14.2)
S2h=c(196.70, 379.16, 1340.31, 18.57)

Vystbar=sum(Nh^2*S2h/(N^2*nh))-sum(Nh*S2h/(N^2))
yst=sum(Nh*yh/N)
Vsrs=(N-n)/(n*(N-1))*(sum(Nh*S2h/N)-sum(Nh*S2h/(N*nh))+sum(Nh*yh^2/N)-yst^2+Vystbar)
deff=Vystbar/Vsrs
print(deff)


Nh=c(86, 72, 52, 30)
N=sum(Nh)
nh=c(14, 12, 9, 5)
n=sum(nh)
mydata=c(97, 67, 42, 125, 25, 92, 105, 86, 27, 43, 45, 59, 53, 52,
         125, 155, 67, 96, 256, 47, 310, 236, 220, 352, 142, 190,
         142, 256, 310, 440, 495, 510, 320, 396, 196,
         167, 655, 220, 540, 780)
stra    =c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
           3, 3, 3, 3, 3, 3, 3, 3, 3,
           4, 4, 4, 4, 4)

data2=stra.srs.mean2(Nh, mydata, stra)
yh=data2$mean.stra
S2h=data2$S2h.stra

Vystbar=sum(Nh^2*S2h/(N^2*nh))-sum(Nh*S2h/(N^2))
yst=sum(Nh*yh/N)
Vsrs=(N-n)/(n*(N-1))*(sum(Nh*S2h/N)-sum(Nh*S2h/(N*nh))+sum(Nh*yh^2/N)-yst^2+Vystbar)
deff=Vystbar/Vsrs
print(deff)