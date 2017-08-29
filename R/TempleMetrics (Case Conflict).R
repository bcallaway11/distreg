dr <- function(formla, data, yvals=NULL) {
    ##form <- as.formula(formla)
    dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    y <- dta[,1]
    if (is.null(yvals)) {
        yvals <- unique(y)
    }
}

dr.inner <- function(y, x) {
    glm(y ~ x, method=binomial(link=logit))
}
