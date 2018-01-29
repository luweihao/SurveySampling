##����R�ľ�ֵ����ֵ�����������

source("conf_interval.R")
source("ratio.r")

data.ratio=read.csv("data.ratio.csv")

re1=ratio(data.ratio$distance, data.ratio$mass, N=NULL, auxiliary=NULL, alpha=0.1)
print(re1)


##��֪xbar������ #YBAR# =R*xbar�ľ�ֵ,��ֵ����,�������估deff

source("conf_interval.R")
source("ratio.mean.r")

data.YM=read.csv("data.YM.csv")

re5.51=ratio.mean(data.YM$after, data.YM$before, N=452, auxiliary=216256/452, alpha=0.05)
print(re5.51)


##��֪xbar������ #YTOTAL# =N*R*xbar�ľ�ֵ,��ֵ����,�������估deff

source("conf_interval.R")
source("ratio.total.r")

data.YT=read.csv("data.YT.csv")

re5.51=ratio.total(data.YT$present, data.YT$previous, N=687, auxiliary=70523.16/687, alpha=0.05)
print(re5.51)