## #' @title dr
## #'
## #' @description run single distribution regression
## #'
## #' @inheritParams distreg
## #' @param y a particular value of y
## #' @param x a paricular vector of values for x
## #'
## #' @return numeric value for F(y|x)
## #' @keywords internal
## #' @export
## dr <- function(y, x, data, yname, xnames) {
##     ##form <- as.formula(formla)
##     ##dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
##     x <- data.frame(t(x))
##     colnames(x) <- xnames
##     lgit <- drs.inner(y, data, yname, xnames)
##     predict(lgit, newdata=x, type="response")
## }

#' @title drs
#'
#' @description run multiple distribution regressions
#'
#' @inheritParams distreg
#'
#' @return list of glm objects for each value of y
#' @keywords internal
#' @export
drs <- function(yvals, data, yname, xnames) {
    lapply(yvals, drs.inner, data=data, yname=yname, xnames=xnames)
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
drs.inner <- function(y, data, yname, xnames) {
    IY <- 1*(data[,yname] <= y)
    X <- data[,xnames]
    dta <- cbind.data.frame(IY, X)
    colnames(dta) <- c("IY", xnames)
    formla <- as.formula(paste0("IY ~", paste(xnames, collapse="+")))
    lgit <- glm(formla, data=dta, family=binomial(link=logit))
    lgit
}


#' title distreg
#'
#' @description the main function for running distribution regressions
#'
#' @param yvals all the values of y to compute F(y|x)
#' @param data the dataset
#' @param yname the name of the y variables in the dataset
#' @param xnames the names of the x variables in the dataset
#'
#' @examples
#' data(igm)
#' y0 <- median(igm$lcfincome)
#' distreg(y0, igm, "lcfincome", c("lfincome", "HEDUC"))
#'
#' @return DR object
#' @export
distreg <- function(yvals, data, yname, xnames) {
    DR(yvals, drs(yvals, data, yname, xnames))
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

#' @title Fycondx
#'
#' @description take a particular value of y and predict F(y|x)
#'
#' @param y a particular value of y for F(y|x)
#' @param drobj a distribution regression object
#' @param xdf a dataframe (can contain multiple rows) with x values
#'
#' @return F(y|x) for each value of x passed in
#'
#' @examples
#' data(igm)
#' yvals <- seq(quantile(igm$lcfincome,.05,type=1),
#'  quantile(igm$lcfincome,.95, type=1), length.out=100)
#' dres <- distreg(yvals, igm, "lcfincome", c("lfincome", "HEDUC"))
#' xdf <- data.frame(lfincome=10, HEDUC="COL")
#' y0 <- yvals[50]
#' Fycondx(y0, dres, xdf)
#'
#' @export
Fycondx <- function(y, drobj, xdf) {
    yvals <- drobj$yvals
    glmlist <- drobj$glmlist
    if (! (y %in% yvals)) {
         stop("must provide value of y in drobj$yvals")
    }
    x <- xdf
    colnames(x) <-  names(coef(glmlist[[1]]))[-1]
    i <- which(yvals==y)[1]
    predict(glmlist[[i]], newdata=x, type="response")
}
