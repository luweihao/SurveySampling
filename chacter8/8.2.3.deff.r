##deff:example8.1

N=512

n=12
M=8
yi=c(188, 180.5, 149.75, 207.875, 244.25, 278.50,
     182.75, 211.50, 253.125, 191.125, 274.75, 258.375)*M
si=c(27.19, 17.98, 17.32, 29.17, 45.20, 63.87,
     38.77, 27.48, 44.52, 28.29, 43.70, 43.52)

s2b=M*var(yi/M)
s2w=sum(si^2)/n

pc=(s2b-s2w)/(s2b+(M-1)*s2w)
if(is.null(N)){
  deff=1+(M-1)*pc
}else{
  deff=(1+(M-1)*pc)*(N*M-1)/(M*(N+1))
}

print(pc)
print(deff)


##example8.3

N=NULL

n=c(9, 7)
M=c(740/9, 531/7)

s2b=c(4452, 3185)
s2w=c(2707, 2321)

pc=(s2b-s2w)/(s2b+(M-1)*s2w)
if(is.null(N)){
  deff=1+(M-1)*pc
}else{
  deff=(1+(M-1)*pc)*(N*M-1)/(M*(N+1))
}

print(pc)
print(deff)
