set.seed(347)
n=c(10,100,1000)
nr=10000
nn=length(n)
mu=0
sig=0.25
tec1=matrix(rep(0,nn*nr),ncol=nn)
tec2=tec1
eqm1=rep(0,nn)
eqm2=eqm1
for(i in 1:nn){
for(j in 1:nr){
    x=rlnorm(n[i],meanlog = mu,sdlog = sig)
    tec1[j,i]=mean(x)
    tec2[j,i]=(max(x)/2)
}
}
e1=tec1-exp(mu+((.25^2)/2))
e2=tec2-exp(mu+((.25^2)/2))
eqm1[i]=sum(e1[,i]^2)/nr
eqm2[i]=sum(e2[,i]^2)/nr
boxplot(e1[,1],e2[,1],e1[,2],e2[,2],e1[,3],e2[,3])


