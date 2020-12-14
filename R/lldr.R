## kernel function
#' @title k
#'
#' @description kernel function
#'
#' @param z evaluate kernel at
#' @param h bandwidth
#' @param type either "gaussian" or "epanechnikov"
#'
#' @return k(z/h)
#'
#' @keywords internal
#' @export
k <- function(z,h=1,type="gaussian") {
    u <- z/h
    if (type=="gaussian") {
        dnorm(u)*(abs(u)<=1) ## gaussian
    } else if (type=="epanechnikov") {
        0.75*(1-u^2)*(abs(u)<=1) ## epanechnikov
    }
}

#' @title G
#'
#' @description vectorized version of logit function
#'
#' @param z compute G(z)
#'
#' @return exp(z)/(1+exp(z))
#'
#' @keywords internal
#' @export
G <- function(z) {
    vapply(z, FUN=function(x) {
        if (exp(x) == Inf) {
            return(1)
        } else {
            exp(x)/(1+exp(x))
        }
    }, 1.0)
}

#' @title dg
#'
#' @description derivative of logit function
#'
#' @param z value to compute g(z)
#'
#' @return exp(z)/((1+exp(z)^2)
#'
#' @keywords internal
#' export
dg <- function(z) {
    exp(z)/((1+exp(z))^2)
}

#' @title wll
#'
#' @description weighted log likelihood function for local logit model
#'  for scalar x only
#'
#' @param bet value of parameters
#' @param y vector of y values
#' @param x vector of x values
#' @param thisx scalar value of x for which evaluating
#' @param h bandwidth
#'
#' @return negative of log likelihood function
#'
#' @keywords internal
#' @export
wll <- function(bet,y,xmain,xother=NULL,thisx,h) {
    ##idx <- bet[1] + bet[2]*x ## original way
    x <- cbind(1,xmain,xother)
    bet <- as.matrix(bet)
    idx <- x%*%bet
    -sum( (y*log(G(idx)) + log((1-G(idx)))*(1-y)) * k(xmain-thisx,h) )
}

#' @title wscore
#'
#' @description weighted score function
#'
#' @inheritParams wll
#'
#' @return weighted score
#'
#' @keywords internal
#' @export
wscore <- function(bet,y,xmain,xother=NULL,thisx,h) {
    x <- cbind(1,xmain,xother)
    bet <- as.matrix(bet)
    idx <- x%*%bet
    ##idx <- bet[1] + bet[2]*x
    ##X <- cbind(1,x)
    X <- x
    kh <- k(xmain-thisx,h)
    as.numeric( ( as.numeric(y*(dg(idx)/G(idx))) - (1-y)*(dg(idx)/(1-G(idx))) )*kh )*X
}


#' @title wgr
#'
#' @description weighted gradient (wscore takes care of weights)
#'
#' @inheritParams wll
#'
#' @return weighted gradient
#'
#' @keywords internal
#' @export
wgr <- function(bet,y,xmain,xother=NULL,thisx,h) {
    colSums(-wscore(bet,y,xmain,xother,thisx,h))
    ##colSums(-as.numeric(y*(g(X%*%bet)/G(X%*%bet    )))*X + as.numeric((1-y)*(g(X%*%bet)/(1-G(X%*%bet))))*X)
}



#' @title lldr.inner
#'
#' @description This calculates a single distribution regression for one value
#'  of y and one value of xmain
#'
#' @param xmain a particular value for the "main" continuous x variable
#' @param y a particular value of y to compute local linear distribution
#'  regression for
#' @param Y a vector containing the data for the outcome
#' @param XMain a vector containing the data for the "main" x variable
#' @param XOther a matrix or data.frame containing the data for the "other"
#'  x variables
#' @param h optional bandwidth
#' @param method "level" or "rank" determining whether method should
#'  be used conditional on ytmin1 or the rank of ytmin1
#'
#' @return an llDR object
#'
#' @examples
#' data(igm)
#' lcinc <- 10
#' Y <- igm$lcfincome
#' XMain <- igm$lfincome
#' XOther <- data.frame(COL=1*(igm$HEDUC=="COL"))
#' lldr.inner(lcinc, 10, Y, XMain, XOther)
#' 
#' @export
lldr.inner <- function(xmain, y, Y, XMain, XOther=NULL, h=NULL, method="level") {
    n <- length(Y)
    if (!is.null(XOther)) XOther <- as.matrix(XOther)
    X <- cbind(1,XMain,XOther)
    ##x <- as.matrix(c(1,xmain,xother))
    if (is.null(h)) {
        h <- 1.06*sd(XMain)*n^(-1/4) ## check that this is right
    }
    
    IY <- 1*(Y <= y)
    o <- optim(rep(0,ncol(X)), wll, gr=wgr, y=IY, xmain=XMain, xother=XOther, thisx=xmain, h=h,##Y0tmin1, thisx=ytmin1, h=h,##thisx=ytmin1, h=h,
               control=list(maxit=1000, reltol=1e-2),
               method="BFGS")
    thet <- as.matrix(o$par)
    llDR(y,xmain,thet)
}

#' @title lldr.inner.xvals
#'
#' @description calls lldr.inner for a vector of x values
#'
#' @inheritParams lldr.inner
#' @param xmain.seq a vector of x values to compute F(y|x) using local
#'  linear distribution regression
#'
#' @return a list of llDR objects
#'
#' @keywords internal
#' @export
lldr.inner.xvals <- function(xmain.seq, y, Y, XMain, XOther=NULL, h=NULL, method="level") {
    lapply(xmain.seq, lldr.inner, y=y, Y=Y, XMain=XMain,
           XOther=XOther, h=h, method=method)
}

#' @title lldr.inner.y
#'
#' @description calls lldr.inner.xvals for a particular value of y.  This is a
#'  dummy function that just reverses the arguments of lldr.inner.xvals in
#'  order to be able to call it for a vector of y values
#'
#' @inheritParams lldr.inner.xvals
#'
#' @return a list of llDR objects
#'
#' @keywords internal
#' @export
lldr.inner.y <- function(y, xmain.seq, Y, XMain, XOther=NULL, h=NULL, method="level") {
    lldr.inner.xvals(xmain.seq, y, Y, XMain, XOther, h, method)
}


#' @title lldrs.inner
#'
#' @description internal function for running local linear distribution
#'  regression for a vector of y and x
#'
#' @inheritParams lldr.inner.xvals
#' @param y.seq a vector of y values to compute F(y|x) for using local linear
#'  distribution regression
#' @param cl The number of clusters to use, default is 1
#'
#' @return a list of llDR objects that are indexed by y and t separately
#' 
#' @export
lldrs.inner <- function(y.seq, xmain.seq, Y, XMain, XOther=NULL, h=NULL, method="level", cl=1) {
    out <- pbapply::pblapply(y.seq, lldr.inner.y, xmain.seq=xmain.seq,
                             Y=Y, XMain=XMain,
                             XOther=XOther, h=h, method=method, cl=cl)
    out <- unlist(out, recursive=FALSE)
    class(out) <- "llDRlist"
    out
}

  
