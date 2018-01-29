##3.3 总体比例的简单估计

source("conf_interval.r")
source("proportionmean.r")

#######
N=100             #Total Number
alpha=0.05        #Conf.Level=1-a

mydata=c(1,0,1,0,1,0)

if(FALSE){
  data0=read.csv("hw2.csv")
  mydata =as.matrix(data0[,2])
}

proportion.result=proportionmean(N, mydata, alpha=0.05)

s=paste("The estimated proportion of the data is ", proportion.result$mean.est,
         ", and the 95% confident interval is ", proportion.result$ci, sep = "")
print(s, quote = FALSE)