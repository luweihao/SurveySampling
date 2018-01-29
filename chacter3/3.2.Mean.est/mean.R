##3.2 总体均值与均值方差的简单估计

source("conf_interval.r")
source("mean_srs.r")

#######
N=100             #Total Number
alpha=0.05        #Conf.Level=1-a

##data=c(28.4,5435,33.4,55.123)
data0=read.csv("hw2.csv")
mydata =as.matrix(data0[,2])

mean.result=mean.srs(N, mydata, alpha=0.05)

s1=paste("The mean of the data is ", mean.result$mean.est,
         ", the var of the data is ", mean.result$var.est, sep = "")
s2=paste("And the 95% confident interval is ", mean.result$ci, sep = "")
print(s1, quote = FALSE)
print(s2, quote = FALSE)