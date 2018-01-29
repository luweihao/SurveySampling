source("stra.srs.mean.r")
source("ratio.mean.r")
source("regression.mean.r")

mydata.p=read.csv("pro2.p.data.csv")
mydata.m=read.csv("pro2.m.data.csv")

Nh=c(120, 180)
nh=c(6, 9)
Xbar=c(24500/120, 21200/180)

ly=c(mydata.p$ly.p, mydata.m$ly.m)
ty=c(mydata.p$ty.p, mydata.m$ty.m)
stra=c(rep(1, nh[1]), rep(2, nh[2]))

#####
mydata.p=read.csv("ex5.7.p.data.csv")
mydata.m=read.csv("ex5.7.m.data.csv")

Nh=c(135, 1228)
nh=c(15, 20)
Xbar=c(75650/135, 315612/1228)

ly=c(mydata.p$ly.p, mydata.m$ly.m)
ty=c(mydata.p$ty.p, mydata.m$ty.m)
stra=c(rep(1, nh[1]), rep(2, nh[2]))
#####

##sink("pro2.txt")

##question 1
re.srs=stra.srs.mean(Nh, nh, ty, stra)
cat("###The result for simple estimation is:", "\n")
print(re.srs)


##question 2, 3
cat("This is the result for different estimation methods：", "\n")

re.sep.ratio=stra.sep.ratio.mean(ly, ty, stra, Nh, nh, Xbar)
cat("###The result for separate ratio estimation is:", "\n")
print(re.sep.ratio)

re.com.ratio=stra.com.ratio.mean(ly, ty, stra, Nh, nh, Xbar)
cat("###The result for combined ratio estimation is:", "\n")
print(re.com.ratio)

cat("The cor of last year and this year's output is:\n",
    "Plain:", cor(mydata.p$ly.p, mydata.p$ty.p), "\n",
    "Mountain:", cor(mydata.m$ly.m, mydata.m$ty.m), "\n", "\n")

re.sep.reg=stra.sep.regression.mean(ly, ty, stra, Nh, nh, Xbar)
cat("###The result for separate regression estimation is:", "\n")
print(re.sep.reg)

re.com.reg=stra.com.regression.mean(ly, ty, stra, Nh, nh, Xbar)
cat("###The result for combined regression estimation is:", "\n")
print(re.com.reg)

plot(mydata.p)
zp=line(mydata.p)
cat("Plain:\n", "The coefficient of the regression line yp=a+b*xp is:")
print(zp)
abline(coef(zp))

plot(mydata.m)
zm=line(mydata.m)
cat("Mountain:\n", "The coefficient of the regression line ym=a+b*xm is:")
print(zm)
abline(coef(zm))

##差估计
##混合比估计和回归估计