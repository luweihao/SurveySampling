source("conf_interval.R")
source("srs.r")
source("deff.r")
source("ratio.r")
source("ratio.mean.r")
source("ratio.total.r")
source("regression.mean.r")
source("regression.total.r")

## Example 1:Example 5.3
exa1=read.csv("exa3.csv")
print(exa1)
re1=ratio(exa3$distance, exa3$mass, alpha=0.1)
print(re1)


## Example 2:Example 5.4
exa2=read.csv("exa4.csv")
print(exa2)

re2.srs=srs.mean(N=687, exa4$y, alpha=0.05)
print(re2.srs)

plot(exa2)
re2.ratio=ratio.mean(exa2$y, exa2$x, N=687, auxiliary=705, alpha=0.05)
print(re2.ratio)

deff=re2.ratio$ratio.mean.var/re2.srs$mean.var
print(deff)

















## Example 4: Example 5.4
exa4=read.csv("exa4.csv")
print(exa4)

N=687
n=nrow(exa4)

re4.srs=srs.total(N, exa4$y, alpha=0.05)
print(re4.srs)

re4.ratio=ratio.total(exa4$y, exa4$x, N, Xbar=70523.16/N, alpha=0.05)
print(re4.ratio)

re4.reg=regression.total(exa4$y, exa4$x, N, Xbar=70523.16/N, alpha=0.05)
print(re4.reg)

var.result=matrix(c(re4.srs$total.var, re4.ratio$ratio.total.var, re4.reg$reg.total.var))
colnames(var.result)=c("Simple", "Ratio", "Regression")
deff.result=deff(var.result)
print(deff.result)

sink("example5_4.txt")
cat("This is the result for different estimation methods for SRS", "\n")
cat("The full size N=", N, "\n")
cat("The srs size n=", n, "\n")
cat("The alpha=", alpha, "\n")
cat("The result for simple estimation is:", "\n")
print(re4.srs)
cat("The result for ratio estimation is:", "\n")
print(re4.ratio)
cat("The result for regression estimation is:", "\n")
print(re4.reg)
cat("The relative efficiency result is:", "\n")
print(deff.result)


