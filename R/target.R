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
    print(x$estimates)
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

#################
# private methods


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
#' data = data.frame(y = c(0, 1, 2), x1 = c(2, 4, 6), x2 = c(3, 6, 9))
#' t = target(y ~ x1 * x2, data=data)
#' t$point
target.formula <- function(
    formula,
    data=list(),
    distro_family="gaussian",
    mlm=FALSE,
    ...)
{
    terms = unpack_formula(formula)
    zt = calc_zero_target(terms, data)
    obj = list(
        formula = formula,
        call = match.call(),
        terms = terms,
        zero_targeted = zt,
        estimates = unpack_estimates(calc_estimates(zt, mlm, distro_family))
        )
    class(obj) = "target"
    obj
}

unpack_formula <- function(formula) {
    terms = names(attr(terms(formula), "factors")[,1])
    list(
        dv_name = terms[1],
        d1_name = terms[2],
        d2_name = terms[3]
    )
}

calc_zero_target <- function(terms, data) {
    # zt means "zero targeted"
    zt = data.frame(
        #ids = seq(from=1, to=dim(data)[1]),
        dv = data[[terms$dv_name]],
        d1_high = data[[terms$d1_name]] - sd(data[[terms$d1_name]], na.rm=T),
        d1_low = data[[terms$d1_name]] + sd(data[[terms$d1_name]], na.rm=T),
        d2_high = data[[terms$d2_name]] - sd(data[[terms$d2_name]], na.rm=T),
        d2_low = data[[terms$d2_name]] + sd(data[[terms$d2_name]], na.rm=T)
        )
    data.frame(data, zt)
}

calc_estimates <- function(zt, mlm, distro_family) {
    if (mlm) {
        with(zt, list(
            low_low = calc_lmer(dv ~ (1|ids) + d1_low * d2_low, distro_family),
            low_high = calc_lmer(dv ~ (1|ids) + d1_low * d2_high, distro_family),
            high_low = calc_lmer(dv ~ (1|ids) + d1_high * d2_low, distro_family),
            high_high = calc_lmer(dv ~ (1|ids) + d1_high * d2_high, distro_family)
            )
        )
    }
    else {
        with(zt, list(
            low_low = calc_lm(dv ~ d1_low * d2_low, distro_family),
            low_high = calc_lm(dv ~ d1_low * d2_high, distro_family),
            high_low = calc_lm(dv ~ d1_high * d2_low, distro_family),
            high_high = calc_lm(dv ~ d1_high * d2_high, distro_family)
            )
        )
    }
}

unpack_estimates <- function(estimate) {
    list(
        low = list(
            low = list(
                mean = estimate[["low_low"]][["mean"]],
                stderr = estimate[["low_low"]][["stderr"]]
                ),
            high = list(
                mean = estimate[["low_high"]][["mean"]],
                stderr = estimate[["low_high"]][["stderr"]]
                )
            ),
        high = list(
            low = list(
                mean = estimate[["high_low"]][["mean"]],
                stderr = estimate[["high_low"]][["stderr"]]
                ),
            high = list(
                mean = estimate[["high_high"]][["mean"]],
                stderr = estimate[["high_high"]][["mean"]]
                )
            )
        )
}

calc_lm <- function(spec, distro_family) {
    model = glm(spec, na.action="na.exclude", family=distro_family)
    list(
        mean = summary(model)$coefficients[1,1],
        stderr = summary(model)$coefficients[1,2]
    )
}

calc_lmer <- function(spec, distro_family) {
    model = lmer(spec, na.action="na.exclude", family=distro_family)
    list(
        mean = attr(model, 'fixef')[[1]],
        stderr = attr(summary(model), "coefs")[[1,2]]
    )
}
