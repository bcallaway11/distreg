dr <- function(y, x, data, yname, xname) {
    ##form <- as.formula(formla)
    ##dta <- model.frame(terms(formla,data=data),data=data) #or model.matrix
    IY <- 1*(data[,yname] <= y)
    X <- data[,xname]
    predict(glm(IY ~ data[,xname], method=binomial(link=logit)),
            newdata=data.frame(X=x), type="response")
}
