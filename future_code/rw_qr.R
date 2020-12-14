# These are not currently included in the package, but we should add them soon


# estimate H(y,x)

# function used to compute Tn: quantile regression

#' @param origdata holds the original data in the case where this is
#'  a bootstrap iteration; otherwise it is not used
TnFun_qr=function(formula, data, tau, orig.data=data) {
  qr.res <- rq(formula, data=data, tau=tau)
  yname <- BMisc::lhs.vars(formula)

  # setup data
  Y <- data[,yname]
  X <- model.matrix(BMisc::rhs(formula), data=data)
  dat <- cbind(Y,X)

  # setup orig.data
  orig.Y <- orig.data[,yname]
  orig.X <- model.matrix(BMisc::rhs(formula), data=orig.data)
  orig.dat <- cbind(orig.Y,orig.X)
  
  N <- nrow(dat)
  Yhat <- predict(qr.res, type="Fhat", stepfun=TRUE)
  Hn=rep(0,N)
  H0n=rep(0,N)
  listH <- pblapply(1:N, function(i) {
    thisdat <- orig.dat[i,]
    H <- mean(apply(dat, 1, function(row) all(row <= thisdat)))

    #indmat=((dat - rep(dat[i,],n=nrow(dat)))<=0 )
    #H=length(which(rowSums(indmat)==ncol(indmat)))/nrow(indmat)
    Hn[i]=H
    
    #indmatx=as.matrix(indmat[,-1])
    #datt=dat[which(rowSums(indmatx)==ncol(indmatx)),]
    #dattx=as.data.frame(datt[,-1])
    #colnames(dattx) <- colnames(datt)[-1]
    #datty=datt[,1]
    #p=predict(qr.res, newdata=dattx, type="response")
    FYhat <- sapply(Yhat, function(yhat) yhat(orig.Y[i]))
    thisX <- orig.X[i,]
    H0 <- mean(sapply(1:N, function(j) FYhat[j] * all( X[j,] <= orig.X[i,]) ))

    ## predict(qrout, newdata=dta1, type="Qhat", stepfun=TRUE)
    ## H0=sum(rowMeans(p<=datty))/nrow(dat)
    list(H=H, H0=H0)    
  })
  Hn <- unlist(BMisc::getListElement(listH, "H"))
  H0n <- unlist(BMisc::getListElement(listH, "H0"))
  Tn=sum((Hn-H0n)^2)
  Tn
}




# Bootstrap

BootF_qr=function(dat=dat,object=qr,N=N,B=B,taus){
    bootf=function(dat=dat,object=qr,N=N){
      datx=as.matrix(dat[,-1])
      datxb=data.frame(datx[sample(1:nrow(datx), nrow(datx), replace = TRUE),])
      colnames(datxb) <- colnames(dat)[-1]
      U=sample(taus,nrow(datxb),replace = TRUE)
      datyb=rep(0,nrow(datxb))
      pb <- predict(qr, newdata=datxb)
      for (i in 1:nrow(datxb)) {
        ##pb=predict(qr, newdata=as.data.frame(datxb[i,]), type="response")
        datybb=pb[i,which(U[i]==qr$tau)]
        datyb[i]=datybb
      }
      datb=cbind(datyb,datxb)
      
      Tnb_qr=TnFun_qr(dat=datb,object=qr,N=N)
      Tnb_qr
    }
    Tnb=pbsapply(1:B, function(B){bootf(dat=dat,object=qr,N=N)}, cl=6)
    Tnb
}


# critical value
cFun=function(obj,alpha){
  c=quantile(obj,probs =1- alpha)
  c
}




testF_qr=function(formula,taus,dat,B,alpha){
  dat=as.data.frame(dat)
  qr <- rq(formula, tau = taus, data = dat)
  Tn_qr=TnFun_qr(dat=dat,object=qr,N=nrow(dat))
  Tnb_qr=BootF_qr(dat=dat,object=qr,N=nrow(dat),B=B,taus)
  p=1-ecdf(Tnb_qr)(Tn_qr)
  cFun=function(obj,alpha){
    c=quantile(obj,probs =1- alpha)
    c
  }
  critical_qr=as.numeric(cFun(obj=Tnb_qr,alpha=alpha))
  r=c(Tn_qr,p,critical_qr)
  names(r)=c("Tn","P","Cn")
  r
}


