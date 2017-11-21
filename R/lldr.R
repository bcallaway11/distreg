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



#' @title F.Y1
#'
#' @description calculate F(y|ytmin1), the conditional distribution
#'  of treated potential outcomes conditional on ytmin1;
#'  The order of the variables is due to the way that the function
#'  is called later on
#'
#' @param y.seq possible values for y to take
#' @param ytmin1 the value of ytmin1 to condition on
#' @param Y1t vector of outcomes for the treated group in period t
#' @param Y0tmin1 vector of outcomes for the treated group in period t-1
#' @param h optional bandwidth
#' @param method "level" or "rank" determining whether method should
#'  be used conditional on ytmin1 or the rank of ytmin1
#'
#' @return distribution F(y|ytmin1)
#'
#' @examples
#' data(displacements)
#' ytmin1 <- 10
#' Y1t <- subset(displacements, year==2011 & treat==1)$learn
#' Y0tmin1 <- subset(displacements, year==2007 & treat==1)$learn
#' y.seq <- seq(min(c(Y0tmin1,Y1t)), max(c(Y0tmin1,Y1t)), length.out=100)
#' F.Y1(ytmin1, y.seq, Y1t, Y0tmin1)
#' 
#' @export
ll.Fycondx.inner <- function(xmain, y, Y, XMain, XOther=NULL, h=NULL, method="level") {
    n <- length(Y)
    XOther <- as.matrix(XOther)
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

ll.Fycondx.xvals <- function(xmain.seq, y, Y, XMain, XOther=NULL, h=NULL, method="level") {
    lapply(xmain.seq, ll.Fycondx.inner, y=y, Y=Y, XMain=XMain,
           XOther=XOther, h=h, method=method)
}

ll.Fycondx.y <- function(y, xmain.seq, Y, XMain, XOther=NULL, h=NULL, method="level") {
    ll.Fycondx.xvals(xmain.seq, y, Y, XMain, XOther, h, method)
}

ll.Fycondx <- function(y.seq, xmain.seq, Y, Xmain, XOther=NULL, h=NULL, method="level") {
    lapply(y.seq, ll.Fycondx.y, xmain.seq=xmain.seq, Y=Y, XMain=Xmain,
           XOther=XOther, h=h, method=method)
}

  


## ll.Fycondx <- function(xmain, y.seq, Y, XMain, xother=NULL, XOther=NULL, h=NULL, method="level") {
##     n <- length(Y)
##     ## if (method=="rank") {
##     ##     XMain <- order(XMain)/n 
##     ## }
##     ##X <- cbind(1, XMain - xmain)
##     x <- as.matrix(c(1,xmain,xother))
##     if (is.null(h)) {
##         h <- 1.06*sd(XMain)*n^(-1/4) ## check that this is right
##     }
##     ##h <- h/5
##     ##K <- diag(k(XMain - xmain,h), n, n)

##     Fycondx.vals <- vapply(y.seq, FUN=function(y) {
##             IY <- 1*(Y <= y)
##             ## (solve(t(X)%*%K%*%X) %*% t(X)%*%K%*%IY)[1] ##local linear
##             ##predict(glm(IY ~ Y0tmin1, family=binomial(link=logit),
##             ##            weights=k(Y0tmin1-ytmin1,2)),
##             ##        newdata=data.frame(Y0tmin1=ytmin1),
##             ##        type="response") ## local logit, I think weights are treated the wrong way here
##             o <- optim(rep(0,nrow(x)), wll, gr=wgr, y=IY, xmain=XMain, xother=XOther, thisx=xmain, h=h,##Y0tmin1, thisx=ytmin1, h=h,##thisx=ytmin1, h=h,
##                        control=list(maxit=1000, reltol=1e-2),
##                        method="BFGS")
##             thet <- as.matrix(o$par)
##             G(t(x)%*%thet)
##     } , 1.0)

##     ## rearrangement step
##     Fycondx.vals <- Fycondx.vals[order(Fycondx.vals)]
    
##     BMisc::makeDist(y.seq, Fycondx.vals, TRUE)
## }
