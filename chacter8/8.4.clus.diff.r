##8.4Ⱥ��С�����:�κ�8.3��
source("clus.r")

N=96
n=20
yi=c(50, 110, 230, 140, 60, 280, 240, 45, 60, 230,
     140, 130, 70, 50, 10, 60, 280, 150, 110, 120)
mi=c(3, 7, 11, 9, 2, 12, 14, 3, 5, 9,
     8, 6, 3, 2, 1, 4, 12, 6, 5, 8)

re1=clus.diffsize1(N=N, n=n, yi=yi)
cat("�ܵ�20�������Ϊ��")
print(re1$clus.diffsize$total.est)
cat("��׼��Ϊ��")
print(sqrt(re1$clus.diffsize$total.var))
print(re1)

re2=clus.diffsize2(N=N, n=n, yi=yi, mi=mi, M0=740)
cat("�ܵ�740�����������Ϊ��")
print(re2$clus.diffsize$total.est)
cat("��׼��Ϊ��")
print(sqrt(re2$clus.diffsize$total.var))
print(re2)