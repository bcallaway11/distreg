# These are not currently included in the package, but we should add them soon

library(TempleMetrics)

## replicate the simulation results in Rothe and Wied (2013)
rm(list=ls())
setwd('/Users/weigehuang/Dropbox/Int Gen Mobility/R codes')

NS=500
x1=rbinom(NS,1,prob = 0.5)
x2=rnorm(NS)
u=rnorm(NS)
v=(1-2*rbinom(NS,1,prob = 0.5))*rchisq(NS, df=2, ncp = 0)/sqrt(8)
y1=x1+x2+u
y2=x1+x2+v
y3=x1+x2+(0.5+x1)*u
y4=x1+x2+(0.5+x1+x2^2)^(1/2)*u
y5=x1+x2+0.2*(0.5+x1+x2^2)^(3/2)+u

sdatx=cbind(x1,x2)
sdat1=cbind(y1,sdatx)
sdat2=cbind(y2,sdatx)
sdat3=cbind(y3,sdatx)
sdat4=cbind(y4,sdatx)
sdat5=cbind(y5,sdatx)

S=10
sdatlist=list()
for (i in 1:S) {
  x1=rbinom(NS,1,prob = 0.5)
  x2=rnorm(NS)
  u=rnorm(NS)
  v=(1-2*rbinom(NS,1,prob = 0.5))*rchisq(NS, df=2, ncp = 0)/sqrt(8)
  y1=x1+x2+u
  y2=x1+x2+v
  y3=x1+x2+(0.5+x1)*u
  y4=x1+x2+(0.5+x1+x2^2)^(1/2)*u
  y5=x1+x2+0.2*(0.5+x1+x2^2)^(3/2)+u
  
  sdatx=cbind(x1,x2)
  sdat1=cbind(y1,sdatx)
  sdat2=cbind(y2,sdatx)
  sdat3=cbind(y3,sdatx)
  sdat4=cbind(y4,sdatx)
  sdat5=cbind(y5,sdatx)
  sdatlist[[i]]=list(sdat1=sdat1,sdat2=sdat2,sdat3=sdat3,sdat4=sdat4,sdat5=sdat5)
}


TnFun_dr=function(dat,object=dr){
  N=nrow(dat)
  Hn=rep(0,N)
  H0n=rep(0,N)
  for (n in 1:N) {
    indmat=(dat - rep(dat[n,],n=nrow(dat))<=0 )
    H=length(which(rowSums(indmat)==ncol(indmat)))/nrow(indmat)
    
    indmatx=as.matrix(indmat[,-1])
    datt=dat[which(rowSums(indmatx)==ncol(indmatx)),]
    dattx=as.matrix(datt[,-1])
    datty=datt[,1]
    
    l=dr$glmlist
    p=predict(l[[n]], newdata=as.data.frame(dattx), type="response") 
    H0=sum(p)/nrow(indmat)
    
    Hn[n]=H
    H0n[n]=H0
  }
  Tn=sum((Hn-H0n)^2)
  Tn
}

# Bootstrap

BootF=function(dat=dat,object=dr,B=B,alpha,cl=1){
  bootf=function(dat=dat,object=dr){
    datx=as.matrix(dat[,-1])
    datxb=datx[sample(1:nrow(datx), nrow(datx), replace = TRUE),]
    datxb=as.data.frame(datxb)
    
    fyxb=Fycondx(object=dr, xdf=datxb)
    n <- nrow(datxb)
    U <- runif(n)
    Yb <- sapply(1:n, function(i) quantile(fyxb[[i]], probs=U[i]))
    ## l=dr$glmlist
    ## pp=matrix(0,nrow = nrow(datxb),ncol = length(l))
    ## for (i in 1:length(l)) {
    ##   pp[,i]=predict(l[[i]], newdata=as.data.frame(datxb), type="response")
    ## }
    ## ppp=rowMeans(pp)
    ## rr=cbind(sort(y_0),sort(ppp))
    ## Yb=rep(0,nrow(datx))
    ## for (i in 1:nrow(datxb)) {
    ##   U=runif(1, min = 0, max = 1)
    ##   if (max(rr[,2])<=U){
    ##     Yb[i]=max(rr[,1])
    ##   } else
    ##     Yb[i]=rr[,1][min(which(rr[,2]>=U))]
    ## }
    
    datb=cbind(Yb,datxb)
    Tnb_dr=TnFun_dr(dat=datb,object=dr)
    Tnb_dr
  }
  #Tnb=sapply(1:B, function(B){bootf(dat=dat,object=dr,N=N)})
  Tnb=pbapply::pbsapply(1:B, function(B){bootf(dat=dat,object=dr)}, cl=cl)
  Tnb
}

# critical value
cFun=function(obj,alpha){
  c=quantile(obj,probs =1- alpha)
  c
}


testF_dr=function(formula,dat,B,alpha,cl=1){
  dat=as.data.frame(dat)
  yvals=dat[,1]
  N=nrow(dat)
  dr=distreg(formla=formula, data=dat, yvals=yvals, link = "logit")
  Tn_dr=TnFun_dr(dat=dat,object=dr)
  Tnb_dr=BootF(dat=dat,object=dr,B=B,cl=cl)
  p=1-ecdf(Tnb_dr)(Tn_dr)
  cFun=function(obj,alpha){
    c=quantile(obj,probs =1- alpha)
    c
  }
  critical_dr=as.numeric(cFun(obj=Tnb_dr,alpha=alpha))
  r=c(Tn_dr,p,critical_dr)
  names(r)=c("Tn","P","Cn")
  r
}


formula=y5 ~ x1 +x2
dat=as.data.frame(sdat5)
#dat=dat[order(dat$y5),]
#y_0=dat[,1]
y_0=sort(y5)##seq(-4,10,length.out=500)##y5
dr=distreg(formla=formula, data=dat, yvals=y_0, link = "logit")

N=nrow(dat)
Tn_dr=TnFun_dr(dat=dat,object=dr)
Tn_dr

alpha=0.05
#dat=as.data.frame(sdat5)
rr=testF_dr(formula,dat,B=100,alpha,cl=4)
rr

# function used to do simulations
sF_dr=function(formula,B,alpha,S=5,NS=100,whichy="y1",cl=cl){
  datlist=list(list())
  for (i in 1:S) {
    x1=rbinom(NS,1,prob = 0.5)
    x2=rnorm(NS)
    u=rnorm(NS)
    v=(1-2*rbinom(NS,1,prob = 0.5))*rchisq(NS, df=2, ncp = 0)/sqrt(8)
    y1=x1+x2+u
    y2=x1+x2+v
    y3=x1+x2+(0.5+x1)*u
    y4=x1+x2+(0.5+x1+x2^2)^(1/2)*u
    y5=x1+x2+0.2*(0.5+x1+x2^2)^(3/2)+u
    
    sdatx=cbind(x2,x1)
    sdat1=cbind(y1,sdatx)
    colnames(sdat1)=colnames(dat)
    sdat2=cbind(y2,sdatx)
    colnames(sdat2)=colnames(dat)
    sdat3=cbind(y3,sdatx)
    colnames(sdat3)=colnames(dat)
    sdat4=cbind(y4,sdatx)
    colnames(sdat4)=colnames(dat)
    sdat5=cbind(y5,sdatx)
    colnames(sdat5)=colnames(dat)
    if (whichy==y1){datlist[[i]]=sdat1}
    else if (whichy==y2) {datlist[[i]]=sdat2}
    else if (whichy==y3) {datlist[[i]]=sdat3}
    else if (whichy==y4) {datlist[[i]]=sdat4}
    else {datlist[[i]]=sdat5}
  }
  pbapply::pbsapply(1:length(datlist), function(i){testF_dr(formula,dat=datlist[[i]],B,alpha,cl=cl)}, cl=cl)
}


testF_dr(formula,dat=sdat1,B,alpha,cl=cl)

B=100
alpha=0.05
S=10
NS=100

s1=sF_dr(formula,B,alpha,S,NS,whichy="y1",cl=7)
s1
s2=sF_dr(formula,B,alpha,S,NS,whichy="y2",cl=7)
s2
s3=sF_dr(formula,B,alpha,S,NS,whichy="y3",cl=7)
s3
s4=sF_dr(formula,B,alpha,S,NS,whichy="y4",cl=7)
s4
s5=sF_dr(formula,B,alpha,S,NS,whichy="y5",cl=7)
s5
slist_dr=list(s1=s1,s2=s2,s3=s3,s4=s4,s5=s5)

