## 4.2.1.对总体均值的估计

source("stra.srs.mean.r")
source("stra.srs.mean2.r")
source("stratified.sampling.r")

##1：例4.1
Nh=c(23560, 148420)
nh=c(300, 250)
yh=c(15180, 9856)
S2h=c(3972^2, 2546^2)
alpha=0.1
re1=stra.srs.mean1(Nh, nh, yh, S2h, alpha)
print(re1)

##2：课后4.4
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

re2=stra.srs.mean2(Nh, nh, mydata, stra, alpha)
print(re2)


mydata=iris
mydata$stra=0
mydata$stra[mydata$Species=="setosa"]    =1
mydata$stra[mydata$Species=="versicolor"]=2
mydata$stra[mydata$Species=="virginica"] =3

Nh=c(50, 50, 50)
nh=c(25, 25, 25)
alpha=0.05

stra.sample=stra.sampling(Nh, nh, mydata)
print(stra.sample)
mysample=stra.sample$Sepal.Width
stra=stra.sample$stra

re3=stra.srs.mean2(Nh, nh, mysample, stra, alpha)
print(re3)
