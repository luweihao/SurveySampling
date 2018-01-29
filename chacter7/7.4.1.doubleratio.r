##7.4.1二重比估计:example7.2
source("ratio.mean.r")

N=200
x.sample=c(550, 720, 1500, 1020, 620, 980, 928,
           1200, 1350, 1750, 670, 729, 1530)
y.sample=c(610, 780, 1600, 1030, 600, 1050, 977,
           1440, 1570, 2210, 980, 865, 1710)

re=ratio.mean(y.sample, x.sample, n0=80, auxiliary=1080)
print(re)
