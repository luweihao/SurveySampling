##3.2 总体总量与总量方差的简单估计

source("conf_interval.r")
source("population.r")

#######
N=5443             #Total Number
alpha=0.05        #Conf.Level=1-a

##data=c(28.4,5435,33.4,55.123)
data0=read.csv("hw2.csv")
mydata =as.matrix(data0[,2])


population.result=population(N, mydata, alpha=0.05)

s1=paste("The total estimate is ", population.result$total.est,
         ", the var of the estimate is ", population.result$var.est, sep = "")
s2=paste("And the 95% confident interval is ", population.result$ci, sep = "")
print(s1, quote = FALSE)
print(s2, quote = FALSE)