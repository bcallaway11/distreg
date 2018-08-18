#' @title llscm
#'
#' @description local linear estimator of smoothing coefficient model
#'
#' @param t conditional at a value T=t
#' @param Y outcome variable
#' @param T treatment variable
#' @param Xmat covariates
#' @param h bandwidth
#'
#' @return Coefficients
#' @export

## local linear smooth varying coefficient model
llscm <- function(t, Y, T, Xmat, h) {
    X <- cbind(Xmat, (T - t)*Xmat)
    y <- as.matrix(Y)
    n <- length(T)
    if (is.null(h)) {
        h <- 1.06*sd(T)*n^(-1/5) ## check that this is right
    }
    K <- diag(TempleMetrics::k(T-t, h=h, type="gaussian"))
    Coefficients <- solve(t(X)%*%K%*%X)%*%t(X)%*%K%*%y
    Coefficients
}

