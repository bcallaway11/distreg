#' @title drs
#'
#' @description run multiple distribution regressions
#'
#' @inheritParams distreg
#'
#' @return list of glm objects for each value of y
#' @keywords internal
#' @export
drs <- function(yvals, data, yname, xnames, link="logit") {
    lapply(yvals, drs.inner, data=data, yname=yname, xnames=xnames, link=link)
}

#' @title drs.inner
#'
#' @description internal function that does the heavy lifting
#'  on estimating distribution regressions
#'
#' @inheritParams distreg
#' @inheritParams drs
#' @inheritParams dr
#'
#' @return glm object
#' @keywords internal
#' @export
drs.inner <- function(y, data, yname, xnames, link="logit") {
    IY <- 1*(data[,yname] <= y)
    X <- data[,xnames]
    dta <- cbind.data.frame(IY, X)
    colnames(dta) <- c("IY", xnames)
    formla <- as.formula(paste0("IY ~", paste(xnames, collapse="+")))
    lgit <- glm(formla, data=dta, family=binomial(link=link))
    lgit
}


#' @title distreg
#'
#' @description the main function for running distribution regressions
#'
#' @param formla y ~ x
#' @param data the dataset
#' @param yvals all the values of y to compute F(y|x)
#' @param link which link function to use, it can be anything accepted
#'  by glm (for example, logit, probit, or cloglog), the default is "logit"
#'
#' @examples
#' data(igm)
#' y0 <- median(igm$lcfincome)
#' distreg(lcfincome ~ lfincome + HEDUC, igm, y0)
#'
#' @return DR object
#' @export
distreg <- function(formla, data, yvals, link="logit") {
    formla <- as.formula(formla)
    dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    yname <- colnames(dta)[1]
    xnames <- colnames(dta)[-1]
    DR(yvals, drs(yvals, data, yname, xnames, link))
}

## DR class
#'@title DR
#'
#' @description DR (distribution regression) objects
#'
#' @param yvals the values of the y of F(y|x)
#' @param glmlist an estimated model for each y value for F(y|x)
#'
#' @export
DR<- function(yvals, glmlist) {
    out <- list(yvals=yvals, glmlist=glmlist)
    class(out) <- "DR"
    out
}

#' @title Fycondx.DR
#'
#' @description take a particular value of y and predict F(y|x)
#'
#' @importFrom BMisc makeDist
#'
#' @param object a distribution regression object
#' @param yvals the values to compute the ecdf for
#' @param xdf a dataframe (can contain multiple rows) with x values
#'
#' @return an ecdf for each value of the x's
#'
#' @examples
#' data(igm)
#' yvals <- seq(quantile(igm$lcfincome,.05,type=1),
#'  quantile(igm$lcfincome,.95, type=1), length.out=100)
#' dres <- distreg(lcfincome ~ lfincome + HEDUC, igm, yvals)
#' xdf <- data.frame(lfincome=10, HEDUC=c("LessHS","HS"))
#' d <- Fycondx(dres, yvals, xdf)
#' d
#' y0 <- yvals[50]
#' d[[1]](y0)
#'
#' @export
Fycondx.DR <- function(object, yvals, xdf) {
    drobj <- object
    yvals <- drobj$yvals
    glmlist <- drobj$glmlist

    predmat <- sapply(glmlist, function(g) { predict(g, newdata=xdf, type="response") } )  ## a matrix of F(y|x) for all the values in yvals and xdf

    if (class(predmat)=="numeric") {
        predmat <- t(predmat)
    }

    out <- lapply(1:nrow(predmat), function(i) {
        makeDist(yvals, predmat[i,], rearrange=TRUE) } )## should get 3727 distribution functions

    out
}

#' @title Fycondx.llDRlist
#'
#' @description take a particular value of y and predict F(y|x)
#'
#' @inheritParams Fycondx
#'
#' @return a list of ecdfs for each row in xdf
#'
#' @export
Fycondx.llDRlist <- function(object, yvals, xdf) {

    drlist <- object

    ##mapply(drmat, 1, function(llDRobj) {

    t <- unique(xdf[,2])

    whichl <- sapply(drlist, function(d) {d$t==t} )

    thisdr <- drlist[whichl]

    X <- as.matrix(xdf)
    predmat <- sapply(thisdr, function(d) { G(X%*%d$thet) })
        

    if (class(predmat)=="numeric") {
        predmat <- t(predmat)
    }

    out <- lapply(1:nrow(predmat), function(i) {
        makeDist(yvals, predmat[i,], rearrange=TRUE) } )## should get 3727 distribution functions

    out
}


#' @title Fycondx.rqs
#'
#' @description compute the conditional distribution of y conditional on x
#'  using quantile regression
#'
#' @param object a quantile regression object that has been estimated in a
#'  first step
#' @inheritParams Fycondx.DR
#'
#' @return a list of conditional distributions
#' @export
Fycondx.rqs <- function(object, yvals, xdf) {
    qrobj <- object
    Fycondx <- predict(qrobj, newdata=xdf, type="Fhat", stepfun=TRUE)
    Fycondx <- lapply(Fycondx, function(x) { makeDist(yvals, x(yvals), rearrange=TRUE) })  
    Fycondx
    ##lapply(Fycondx, function(x) { x(yvals) })
}

#' @title Fycondx
#'
#' @description a generic method for computing conditional distributions
#'
#' @param object either a distribution regression or quantile regression object
#' @inheritParams Fycondx.DR
#'
#' @return a list of conditional distributions
#'
#' @export
Fycondx <- function(object, yvals, xdf) {
    UseMethod("Fycondx", object)
}

## predict.DR <- function(y, drobj, xdf) {
##     yvals <- drobj$yvals
##     glmlist <- drobj$glmlist
##     if (! (y %in% yvals)) {
##          stop("must provide value of y in drobj$yvals")
##     }
##     x <- xdf
##     colnames(x) <-  names(coef(glmlist[[1]]))[-1]
##     i <- which(yvals==y)[1]
##     predict(glmlist[[i]], newdata=x, type="response")
## }

## #' @title lldrs
## #'
## #' @description run multiple "local" distribution regressions
## #'
## #' @inheritParams distreg
## #'
## #' @return ******
## #' @keywords internal
## #' @export
## lldrs <- function(yvals, data, yname, xnames, link="logit") {
##     lapply(yvals, lldrs.inner, data=data, yname=yname, xnames=xnames, link=link)
## }

## #' @title lldrs.inner
## #'
## #' @description internal function that does the heavy lifting
## #'  on estimating "local" distribution regressions
## #'
## #' @inheritParams distreg
## #' @inheritParams drs
## #' @inheritParams dr
## #'
## #' @return ***
## #' @keywords internal
## #' @export
## lldrs.inner <- function(y, data, yname, xnames, link="logit") {
##     IY <- 1*(data[,yname] <= y)
##     X <- data[,xnames]
##     dta <- cbind.data.frame(IY, X)
##     colnames(dta) <- c("IY", xnames)
##     formla <- as.formula(paste0("IY ~", paste(xnames, collapse="+")))
##     lgit <- glm(formla, data=dta, family=binomial(link=link))
##     lgit
## }


#' @title lldistreg
#'
#' @description the main function for running "local" distribution regressions.
#'  This function runs a local regression that is local for a single
#'  (scalar) continuous treatment variable.  It also allows for other variables
#'  but it does not smooth over these variables.
#'
#' @param formla y ~ t , t must be a single continuous variable
#' @param xformla ~x, x are other (non-smoothed) variables
#'  included in the model
#' @param data the dataset
#' @param yvals all the values of y to compute F(y|t,x)
#' @param tvals the values of the continuous treatment to comput F(y|t,x)
#' @param link which link function to use, it can be anything accepted
#'  by glm (for example, logit, probit, or cloglog), the default is "logit"
#' @param cl the number of clusters to use, default is 1
#' 
#' @examples
#' data(igm)
#' lldistreg(lcfincome ~ lfincome, ~HEDUC, igm, 10, 10)
#'
#' @return a list of llDR objects that are indexed by the values in yvals
#'  and tvals
#' @export
lldistreg <- function(formla, xformla=NULL, data, yvals, tvals, link="logit",
                      cl=1) {
    formla <- as.formula(formla)
    dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    yname <- colnames(dta)[1]
    tname <- colnames(dta)[2]
    xnames <- NULL
    ##set up the x variables
    if (!(is.null(xformla))) {
        xformla <- as.formula(xformla)
        xdta <- model.matrix(xformla, data=data)
        xdta <- xdta[,-1]
        xnames <- colnames(xdta)[-1]
    }


    out <-  lldrs.inner(yvals, xmain.seq=tvals, Y=dta[,yname],XMain=dta[,tname],
                        XOther=xdta, cl=cl)

    out
}

## llDR class
#'@title llDR
#'
#' @description llDR (local linear distribution regression) object.
#'  It contains a value for y, a value for t, and a value for the parameters
#'
#' @param y the value of y for which F(y|t,x) was computed
#' @param t the value of t for which F(y|t,x) was computed
#' @param thet the local parameters from the local linear distribution
#'  regression
#'
#' @export
llDR<- function(y, t, thet) {
    out <- list(y=y, t=t, thet=thet)
    class(out) <- "llDR"
    out
}


    
