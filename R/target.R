#' Create a target object.
#'
#' @param x input model function specification
#' @return target object
#' @keywords character
#' @export
target <- function(x, ...) UseMethod("target")

######
# generic handlers

#' Print a zero-targeted model
#' 
#' @param x a zero-targeted model
#' @export
print.target <- function(x, ...)
{
    print(x$estimates)
}

#' Get mean and stderr information about a zero-targeted model
#' 
#' @param object a zero-targeted model
#' @export
summary.target <- function(object, ...)
{
    res = list(summary = summarize(object))
    class(res) <- "summary.target"
res
}

#' Print a zero-targeted summary
#'
#' @param x a zero-targeted summary object
#' @export
print.summary.target <- function(x, ...)
{
    print(x$summary)
}

#' Plot a zero-targeted model +/- 1 standard deviation.
#' 
#' @param x a zero-targeted model object
#' @param raw boolean indicating whether to plot raw values; defaults to FALSE 
#'  (i.e. print scaled rather than raw values)
#' @export
plot.target <- function(x, raw=F, ...) {
    display = summarize(x)
    if (raw) {
        display = transform_raw(display, x$family)
    }
    else {
        display$ymins = display$means - display$ci
        display$ymaxs = display$means + display$ci
    }

    ggplot(display, aes(x=dim2, y=means, group=dim1)) +
        geom_line(aes(linetype=dim1), size=1) +
        geom_point(size=3, fill="white") +
        scale_x_discrete(labels=c(paste("low", x$terms$d2_name), paste("high", x$terms$d2_name))) +
        scale_linetype(labels=c(paste("high", x$terms$d1_name), paste("low", x$terms$d1_name))) +
        coord_cartesian(xlim=c(0.8, 2.2)) +
        xlab(NULL)+
        ylab(x$terms$dv_name) +
        theme_bw() +
        geom_errorbar(aes(ymin=ymins, ymax=ymaxs), alpha=0.3, width=.05) +
        labs(title = paste(x$terms$dv_name, " ~ '", x$terms$d1_name, "' and '", x$terms$d2_name, "'", sep="")) +
        theme(legend.title=element_blank()) +
        theme(legend.position=c(.2, .9)) +
        theme(legend.text = element_text(size = 12)) +
        theme(axis.text.x = element_text(size = 14)) +
        theme(axis.text.y = element_text(size = 14))
}

#################
# private methods

# Unpack an S3 formula to figure out the name of the DV and IVs
#
# @param formula model specification
# @return target object
unpack_formula <- function(formula) {
    terms = names(attr(terms(formula), "factors")[,1])
    list(
        dv_name = terms[1],
        d1_name = terms[2],
        d2_name = terms[3]
    )
}

# undocumented
#
transform_raw <- function(display, family) {
    if (family == "poisson") {
        b0 = display$means - display$ci
        display$ymins = exp(b0)
        b0 = display$means + display$ci
        display$ymaxs = exp(b0)
        b0 = display$means
        display$means = exp(b0)
    }
    else if (family == "binomial") {
        b0 = display$means - display$ci
        display$ymins = exp(b0) / (1 + exp(b0))
        b0 = display$means + display$ci
        display$ymaxs = exp(b0) / (1 + exp(b0))
        b0 = display$means
        display$means = exp(b0) / (1 + exp(b0))
    }
    else {
        display$ymins = display$means - display$ci
        display$ymaxs = display$means + display$ci
    }
    display
}

# undocumented
#
summarize <- function(x, ...) {
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
    display
}

# undocumented
#
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

# undocumented
#
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

# undocumented
#
calc_estimates <- function(f, zt, mlm, family) {
    if (mlm) {
        fn = calc_lmer
    }
    else {
        fn = calc_lm
    }

    list(
        low_low = fn(f$low_low, zt, family),
        low_high = fn(f$low_high, zt, family),
        high_low = fn(f$high_low, zt, family),
        high_high = fn(f$high_high, zt, family)
    )
}

# undocumented
#
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

# undocumented
#
calc_lm <- function(spec, data, family) {
    model = glm(spec, data, na.action="na.exclude", family=family)
    list(
        model = model,
        mean = summary(model)$coefficients[1,1],
        stderr = summary(model)$coefficients[1,2]
    )
}

# undocumented
#
calc_lmer <- function(spec, data, family) {
    model = lmer(spec, data, na.action="na.exclude", family=family)
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
    family="gaussian",
    mlm=FALSE,
    ...)
{
    terms = unpack_formula(formula)
    f_str = as.character(formula)
    formula_str = paste(f_str[2], f_str[1], f_str[3])
    std_formulas = gen_formulas(formula_str, terms)
    zt = calc_zero_target(terms, data)
    models = calc_estimates(std_formulas, zt, mlm, family)
    obj = list(
        formula = formula,
        call = match.call(),
        terms = terms,
        zero_targeted = zt,
        models = models,
        family = family,
        estimates = unpack_estimates(models)
        )
    class(obj) = "target"
    obj
}
