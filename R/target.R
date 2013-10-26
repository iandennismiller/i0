# For now, this is just the code stub from:
# http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf

#' Create a target object.
#'
#' @param x input model function specification
#' y data frame
#' @return target object
#' @keywords character
#' @export
target <- function(x, ...) UseMethod("target")

######
# generic handlers

#' @export
print.target <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
}

#' @export
summary.target <- function(object, ...)
{
    se <- sqrt(diag(object$vcov))
    tval <- coef(object) / se
    TAB <- cbind(Estimate = coef(object),
                 StdErr = se,
                 t.value = tval,
                 p.value = 2*pt(-abs(tval), df=object$df))
    res <- list(call=object$call,
                coefficients=TAB)
    class(res) <- "summary.target"
res
}

#' @export
print.summary.target <- function(x, ...)
{
    cat("Call:\n")
    print(x$call)
    cat("\n")
    printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
}

#' @export
predict.target <- function(object, newdata=NULL, ...)
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

#' Create a target object.
#'
#' @param x input model function specification
#' y data frame
#' @return target object
#' @keywords character
#' @export
#' @examples
#' data(cats, package="MASS")
#' x = cbind(Const=1, Bwt=cats$Bwt)
#' y = cats$Hw
#' mod1 <- target(x, y)
#' print(mod1)
target.default <- function(x, y, ...)
{
    x <- as.matrix(x)
    y <- as.numeric(y)
    est <- linmodEst(x, y)
    est$fitted.values <- as.vector(x %*% est$coefficients)
    est$residuals <- y - est$fitted.values
    est$call <- match.call()
    class(est) <- "target"
est }

#' Create a target object.
#'
#' @param x input model function specification
#' y data frame
#' @return target object
#' @keywords character
#' @export
#' @examples
#' data(cats, package="MASS")
#' print(summary(target(Hwt~Bwt*Sex, data=cats)))
target.formula <- function(formula, data=list(), ...)
{
    mf <- model.frame(formula=formula, data=data)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    y <- model.response(mf)
    est <- target.default(x, y, ...)
    est$call <- match.call()
    est$formula <- formula
    est
}
