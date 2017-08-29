dr <- function(y, x, data, yname, xnames) {
    ##form <- as.formula(formla)
    ##dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    IY <- 1*(data[,yname] <= y)
    X <- data[,xnames]
    dta <- cbind.data.frame(IY, X)
    x <- data.frame(t(x))
    colnames(x) <- xnames
    formla <- as.formula(paste0("IY ~", paste(xnames, collapse="+")))
    lgit <- glm(formla, data=dta, family=binomial(link=logit))
    predict(lgit, newdata=x, type="response")
}
