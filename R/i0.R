
# http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf

#' Create an i0 object.
#'
#' @param x input model function specification
#' y data frame
#' @return i0 object
#' @keywords character
#' @export
i0 <- function(x, ...) UseMethod("i0")

######
# generic handlers

#' Create an i0 object.
#'
print.i0 <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
}

#' Create an i0 object.
#'
summary.i0 <- function(object, ...)
{
    se <- sqrt(diag(object$vcov))
    tval <- coef(object) / se
    TAB <- cbind(Estimate = coef(object),
                 StdErr = se,
                 t.value = tval,
                 p.value = 2*pt(-abs(tval), df=object$df))
    res <- list(call=object$call,
                coefficients=TAB)
    class(res) <- "summary.i0"
res
}

#' Create an i0 object.
#'
print.summary.i0 <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\n")
    printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
}

#' Create an i0 object.
#'
predict.i0 <- function(object, newdata=NULL, ...)
{
    if(is.null(newdata))
      y <- fitted(object)
    else{
        if(!is.null(object$formula)){
            ## model has been fitted using formula interface
            x <- model.matrix(object$formula, newdata)
        }
        else{
            x <- newdata
}
        y <- as.vector(x %*% coef(object))
    }
y }

#################
# private methods

linmodEst <- function(x, y)
{
    ## compute QR-decomposition of x
    qx <- qr(x)
    ## compute (x’x)^(-1) x’y
    coef <- solve.qr(qx, y)
    ## degrees of freedom and standard deviation of residuals
    df <- nrow(x)-ncol(x)
    sigma2 <- sum((y - x%*%coef)^2)/df
    ## compute sigma^2 * (x’x)^-1
    vcov <- sigma2 * chol2inv(qx$qr)
    colnames(vcov) <- rownames(vcov) <- colnames(x)
    list(coefficients = coef,
         vcov = vcov,
         sigma = sqrt(sigma2),
         df = df)
}

################
# public methods

#' Create an i0 object.
#'
#' @param x input model function specification
#' y data frame
#' @return i0 object
#' @keywords character
i0.default <- function(x, y, ...)
{
    x <- as.matrix(x)
    y <- as.numeric(y)
    est <- linmodEst(x, y)
    est$fitted.values <- as.vector(x %*% est$coefficients)
    est$residuals <- y - est$fitted.values
    est$call <- match.call()
    class(est) <- "i0"
est }

#' Create an i0 object.
#'
i0.formula <- function(formula, data=list(), ...)
{
    mf <- model.frame(formula=formula, data=data)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    y <- model.response(mf)
    est <- i0.default(x, y, ...)
    est$call <- match.call()
    est$formula <- formula
    est
}
