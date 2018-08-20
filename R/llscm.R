#' @title llscm.inner
#'
#' @description internal function that does the heavy lifting
#'
#' @param t conditional at a value T=t
#' @param Y outcome variable
#' @param T treatment variable
#' @param Xmat covariates
#' @param h bandwidth
#' @keywords internal
#' @return a 2*k (k being the dimension of X) vector of coefficients, the first
#'  k are the "levels", the second k are the derivatives with respect to each
#'  element of X.
#'
#' @export
#'
#'
llscm.inner <- function(t, Y, T, Xmat, h) {
    X <- cbind(Xmat, (T - t)*Xmat)
    y <- as.matrix(Y)
    n <- length(T)
    if (is.null(h)) {
        h <- 1.06*sd(T)*n^(-1/5) ## check that this is right
    }
    K <- diag(k(T-t, h=h, type="gaussian"))
    Coefficients <- solve(t(X)%*%K%*%X)%*%t(X)%*%K%*%y
    Coefficients
}


#' @title llscm
#'
#' @description local linear estimator of smoothing coefficient model
#' @param formla a formula y ~ treatment
#' @param xformla one sided formula for x variables to include, e.g. ~x1 + x2
#' @param data the data.frame where y, t, and x are
#' @inheritParams llscm.inner
#' @return a 2*k (k being the dimension of X) vector of coefficients, the first
#'  k are the "levels", the second k are the derivatives with respect to each
#'  element of X.
#' @examples
#' data(igm)
#' igm$hs=ifelse(igm$HEDUC=="HS",1,0)
#' igm$col=ifelse(igm$HEDUC=="COL",1,0)
#' formla=lcfincome~lfincome
#' xformla=~hs+col
#' t=mean(igm$lfincome)
#' h=1.2
#' data=igm
#' llscm(formla,xformla,data,t,h)
#' @export
llscm<-function(formla,xformla,data,t,h){
  formla=as.formula(formla)
  xformla=as.formula(xformla)
  YT=model.frame(terms(formla,data=data),data=data)
  X=model.frame(terms(xformla,data=data),data=data)
  Y=YT[,1]
  T=YT[,2]
  Xmat=as.matrix(X)
  out<-llscm.inner(t=t, Y=Y, T=T, Xmat=Xmat, h=h)
  out
}



