##8.4群大小不相等:课后8.3题
source("clus.r")

N=96
n=20
yi=c(50, 110, 230, 140, 60, 280, 240, 45, 60, 230,
     140, 130, 70, 50, 10, 60, 280, 150, 110, 120)
mi=c(3, 7, 11, 9, 2, 12, 14, 3, 5, 9,
     8, 6, 3, 2, 1, 4, 12, 6, 5, 8)

re1=clus.diffsize1(N=N, n=n, yi=yi)
cat("总的20家修理费为：")
print(re1$clus.diffsize$total.est)
cat("标准差为：")
print(sqrt(re1$clus.diffsize$total.var))
print(re1)

re2=clus.diffsize2(N=N, n=n, yi=yi, mi=mi, M0=740)
cat("总的740条带锯修理费为：")
print(re2$clus.diffsize$total.est)
cat("标准差为：")
print(sqrt(re2$clus.diffsize$total.var))
print(re2)