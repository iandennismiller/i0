require(stringr)

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
    # http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
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
    # http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
    cat("Call:\n")
    print(x$call)
    cat("\n")
    printCoefmat(x$coefficients, P.value=TRUE, has.Pvalue=TRUE)
}

#' @export
plot.target <- function(x, ...) {
    display = data.frame(
        dim1 = c('low', 'low', 'high', 'high'),
        dim2 = c('low', 'high', 'low', 'high'),
        means = c(
            x$estimates$low$low$mean,
            x$estimates$low$high$mean,
            x$estimates$high$low$mean,
            x$estimates$high$high$mean
        ),
        ci = c(
            x$estimates$low$low$stderr,
            x$estimates$low$high$stderr,
            x$estimates$high$low$stderr,
            x$estimates$high$high$stderr
        )
    )

    display$dim2 = factor(display$dim2, levels=c('low', 'high'))

    ggplot(display, aes(x=dim2, y=means, group=dim1)) +
        geom_line(aes(linetype=dim1), size=1) +
        geom_point(size=3, fill="white") +
        scale_x_discrete(labels=c(paste("low", x$terms$d2_name), paste("high", x$terms$d2_name))) +
        scale_linetype(labels=c(paste("high", x$terms$d1_name), paste("low", x$terms$d1_name))) +
        coord_cartesian(xlim=c(0.8, 2.2)) +
        xlab(NULL)+
        ylab(x$terms$dv_name) +
        geom_errorbar(aes(ymin=means-ci, ymax=means+ci), alpha=0.3, width=.05) +
        #theme_bw() +
        opts(title = paste(x$terms$dv_name, " ~ '", x$terms$d1_name, "' and '", x$terms$d2_name, "'", sep="")) +
        opts(legend.title=theme_blank()) + opts(legend.position=c(.2, .9)) +
        opts(legend.text = theme_text(size = 12)) +
        opts(axis.text.x = theme_text(size = 14)) +
        opts(axis.text.y = theme_text(size = 14))
}

#################
# private methods

#' Unpack an S3 formula to figure out the name of the DV and IVs
#'
#' @param formula model specification
#' y data frame
#' @return target object
unpack_formula <- function(formula) {
    terms = names(attr(terms(formula), "factors")[,1])
    list(
        dv_name = terms[1],
        d1_name = terms[2],
        d2_name = terms[3]
    )
}

#' undescribed
#'
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

#' undescribed
#'
gen_formulas <- function(formula_str, terms) {
    f_low_low = str_replace_all(formula_str, terms$d1_name, "d1_low")
    f_low_low = str_replace_all(f_low_low, terms$d2_name, "d2_low")

    f_low_high = str_replace_all(formula_str, terms$d1_name, "d1_low")
    f_low_high = str_replace_all(f_low_high, terms$d2_name, "d2_high")

    f_high_low = str_replace_all(formula_str, terms$d1_name, "d1_high")
    f_high_low = str_replace_all(f_high_low, terms$d2_name, "d2_low")

    f_high_high = str_replace_all(formula_str, terms$d1_name, "d1_high")
    f_high_high = str_replace_all(f_high_high, terms$d2_name, "d2_high")

    list(
        low_low = as.formula(f_low_low),
        low_high = as.formula(f_low_high),
        high_low = as.formula(f_high_low),
        high_high = as.formula(f_high_high)
    )
}

#' undescribed
#'
calc_estimates <- function(f, zt, mlm, distro_family) {
    if (mlm) {
        fn = calc_lmer
    }
    else {
        fn = calc_lm
    }

    list(
        low_low = fn(f$low_low, zt, distro_family),
        low_high = fn(f$low_high, zt, distro_family),
        high_low = fn(f$high_low, zt, distro_family),
        high_high = fn(f$high_high, zt, distro_family)
    )
}

#' undescribed
#'
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
                stderr = estimate[["high_high"]][["stderr"]]
                )
            )
        )
}

#' undescribed
#'
calc_lm <- function(spec, data, distro_family) {
    model = glm(spec, data, na.action="na.exclude", family=distro_family)
    list(
        model = model,
        mean = summary(model)$coefficients[1,1],
        stderr = summary(model)$coefficients[1,2]
    )
}

#' undescribed
#'
calc_lmer <- function(spec, data, distro_family) {
    model = lmer(spec, data, na.action="na.exclude", family=distro_family)
    list(
        model = model,
        mean = attr(model, 'fixef')[[1]],
        stderr = attr(summary(model), "coefs")[[1,2]]
    )
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
    f_str = as.character(formula)
    formula_str = paste(f_str[2], f_str[1], f_str[3])
    std_formulas = gen_formulas(formula_str, terms)
    zt = calc_zero_target(terms, data)
    models = calc_estimates(std_formulas, zt, mlm, distro_family)
    obj = list(
        formula = formula,
        call = match.call(),
        terms = terms,
        zero_targeted = zt,
        models = models,
        estimates = unpack_estimates(models)
        )
    class(obj) = "target"
    obj
}
