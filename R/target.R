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
#' print(summary(target(Hwt~Bwt*Sex, data=cats)))
target.formula <- function(formula, data=list(), distro_family="gaussian", ...)
{
    terms = names(attr(terms(formula), "factors")[,1])
    dv_name = terms[1]
    d1_name = terms[2]
    d2_name = terms[3]
    dv = data[[dv_name]]

    # zt means "zero targeted"
    zt = list(
        d1_high = data[[d1_name]] - sd(data[[d1_name]], na.rm=T),
        d1_low = data[[d1_name]] + sd(data[[d1_name]], na.rm=T),
        d2_high = data[[d2_name]] - sd(data[[d2_name]], na.rm=T),
        d2_low = data[[d2_name]] + sd(data[[d2_name]], na.rm=T)
        )

    mean_stderr = list(
        low_low = lm_mean_stderr(dv ~ zt$d1_low * zt$d2_low, distro_family),
        low_high = lm_mean_stderr(dv ~ zt$d1_low * zt$d2_high, distro_family),
        high_low = lm_mean_stderr(dv ~ zt$d1_high * zt$d2_low, distro_family),
        high_high = lm_mean_stderr(dv ~ zt$d1_high * zt$d2_high, distro_family)
        )

    mean = list(
        low_low = mean_stderr[["low_low"]][["mean"]],
        low_high = mean_stderr[["low_high"]][["mean"]],
        high_low = mean_stderr[["high_low"]][["mean"]],
        high_high = mean_stderr[["high_high"]][["mean"]]
        )

    stderr = list(
        low_low = mean_stderr[["low_low"]][["stderr"]],
        low_high = mean_stderr[["low_high"]][["stderr"]],
        high_low = mean_stderr[["high_low"]][["stderr"]],
        high_high = mean_stderr[["high_high"]][["stderr"]]
        )

    obj = list(
        formula = formula,
        call = match.call(),
        mean = mean,
        stderr = stderr
        )
    class(obj) = "target"
    obj
}

lm_mean_stderr <- function(spec, distro_family) {
  model <- lm(spec, na.action="na.exclude")
  ret_mean <- attr(model, 'fixef')[[1]]
  ret_stderr <- attr(summary(model), "coefs")[[1,2]]
  list(mean=ret_mean, stderr=ret_stderr)
}

lmer_mean_stderr <- function(spec, distro_family) {
  model <- lmer(spec, na.action="na.exclude", family=distro_family)
  ret_mean <- attr(model, 'fixef')[[1]]
  ret_stderr <- attr(summary(model), "coefs")[[1,2]]
  list(mean=ret_mean, stderr=ret_stderr)
}
