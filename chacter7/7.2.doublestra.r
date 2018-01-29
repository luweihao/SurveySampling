## 7.2.二重分层抽样

source("stra.srs.mean.r")
source("stra.srs.mean2.r")

##1：例7.1
Nh=c(540, 320, 100, 40)
nh=c(80, 60 ,40, 20)
yh=c(2, 7, 15, 40)
S2h=c(1.01, 2.71, 15.38, 690.53)
alpha=0.1
N=8000
re1=stra.srs.mean1(Nh, nh, yh, S2h, alpha, N)
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
