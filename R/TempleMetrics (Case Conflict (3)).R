dr <- function(y, x, data, yname, xnames) {
    ##form <- as.formula(formla)
    ##dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    IY <- 1*(data[,yname] <= y)
    X <- data[,xnames]
    x <- data.frame(t(x))
    colnames(x) <- xnames
    predict(glm(IY ~ X, family=binomial(link=logit)),
            newdata=x, type="response")
}
