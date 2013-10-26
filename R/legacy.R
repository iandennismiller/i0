library(ggplot2)

just_summary <- function(spec, distro_family) {
  model <- lmer(spec, na.action="na.exclude", family=distro_family)
  print(summary(model))
}

extract_mean <- function(spec, distro_family) {
  model <- lmer(spec, na.action="na.exclude", family=distro_family)
  ret_mean <- attr(model, 'fixef')[[1]]
  ret_stderr <- attr(summary(model), "coefs")[[1,2]]
  return(list(ret_mean, ret_stderr))
}

extract_stderr <- function(model) {
  return(0)
}

simple_slopes_core <- function (ids, dv, dim1, dim2, distro_family) {
  simple_slopes <- data.frame(ids)
  simple_slopes$dim1_high <- dim1 - sd(dim1, na.rm=T)
  simple_slopes$dim1_low <- dim1 + sd(dim1, na.rm=T)
  simple_slopes$dim2_high <- dim2 - sd(dim2, na.rm=T)
  simple_slopes$dim2_low <- dim2 + sd(dim2, na.rm=T)
  return(simple_slopes)
}

# Use Page-Gould's quick means technique to find the 4 means
simple_slopes_means <- function (ids, dv, distro_family, ss) {
  means <- list()
  means$low_low <- extract_mean(dv ~ (1|ids) + ss$dim1_low * ss$dim2_low, distro_family)[[1]]
  means$low_high <- extract_mean(dv ~ (1|ids) + ss$dim1_low * ss$dim2_high, distro_family)[[1]]
  means$high_low <- extract_mean(dv ~ (1|ids) + ss$dim1_high * ss$dim2_low, distro_family)[[1]]
  means$high_high <- extract_mean(dv ~ (1|ids) + ss$dim1_high * ss$dim2_high, distro_family)[[1]]

  stderr <- list()
  stderr$low_low <- extract_mean(dv ~ (1|ids) + ss$dim1_low * ss$dim2_low, distro_family)[[2]]
  stderr$low_high <- extract_mean(dv ~ (1|ids) + ss$dim1_low * ss$dim2_high, distro_family)[[2]]
  stderr$high_low <- extract_mean(dv ~ (1|ids) + ss$dim1_high * ss$dim2_low, distro_family)[[2]]
  stderr$high_high <- extract_mean(dv ~ (1|ids) + ss$dim1_high * ss$dim2_high, distro_family)[[2]]

  return(list(means, stderr))
}

# make sure both dim1 and dim2 have been scaled
# e.g. simple_slopes(scale(closeness, scale=F), scale(predictions, scale=F))
simple_slopes_tests <- function (ids, dv, dim1, dim2, distro_family) {
  ss <- simple_slopes_core(ids, dv, dim1, dim2, distro_family)
  cat("\n########### dim1 high\n\n")
  just_summary(dv ~ (1|ids) + ss$dim1_high * dim2, distro_family)
  cat("\n########### dim1 low\n\n")
  just_summary(dv ~ (1|ids) + ss$dim1_low * dim2, distro_family)
  cat("\n########### dim2 high\n\n")
  just_summary(dv ~ (1|ids) + ss$dim2_high * dim1, distro_family)
  cat("\n########### dim2 low\n\n")
  just_summary(dv ~ (1|ids) + ss$dim2_low * dim1, distro_family)
}

simple_slopes_plot <- function(ids, dv, dim1, dim2, distro_family, dv_name, dim1_name, dim2_name) {
  ss <- simple_slopes_core(ids, dv, dim1, dim2, distro_family)
  means <- simple_slopes_means(ids, dv, distro_family, ss)[[1]]
  stderr <- simple_slopes_means(ids, dv, distro_family, ss)[[2]]

  display <- data.frame(
    dim1 = c('low', 'low', 'high', 'high'),
    dim2 = c('low', 'high', 'low', 'high'),
    means = c(means$low_low, means$low_high, means$high_low, means$high_high),
    ci = c(stderr$low_low, stderr$low_high, stderr$high_low, stderr$high_high)
  )
  display$dim2 <- factor(display$dim2, levels=c('low', 'high'))

  ggplot(display, aes(x=dim2, y=means, group=dim1)) +
    geom_line(aes(linetype=dim1), size=1) +
    geom_point(size=3, fill="white") +
    scale_x_discrete(labels=c(paste("low", dim2_name), paste("high", dim2_name))) +
    scale_linetype(labels=c(paste("high", dim1_name), paste("low", dim1_name))) +
    coord_cartesian(xlim=c(0.8, 2.2)) +
    xlab(NULL)+
    ylab(dv_name) +
    geom_errorbar(aes(ymin=means-ci, ymax=means+ci), alpha=0.3, width=.05) +
    #theme_bw() +
    opts(title = paste(dv_name, " ~ '", dim1_name, "' and '", dim2_name, "'", sep="")) +
    opts(legend.title=theme_blank()) + opts(legend.position=c(.2, .9)) +
    opts(legend.text = theme_text(size = 12)) +
    opts(axis.text.x = theme_text(size = 14)) +
    opts(axis.text.y = theme_text(size = 14))
}

simple_slopes <- function(ids, dv, dim1, dim2, distro_family, dv_name, dim1_name, dim2_name) {
  #simple_slopes_tests(ids, dv, dim1, dim2, distro_family)
  simple_slopes_plot(ids, dv, dim1, dim2, distro_family, dv_name, dim1_name, dim2_name)
}

simple_slopes_3way = function(dv_name, dim1_name, dim2_name, dim3_name, data, family="gaussian") {
  with(data, {
    dv = get(dv_name)
    dim1 = get(dim1_name)
    dim2 = get(dim2_name)
    dim3 = get(dim3_name)

    display = data.frame(dim1 = NA, dim2 = NA, dim3 = NA, means = NA, ci = NA)

    dims = list()
    dims[[1]] = dims[[2]] = dims[[3]] = list()

    dims[[1]][["high"]] = dim1 - sd(dim1, na.rm=T)
    dims[[1]][["low"]] = dim1 + sd(dim1, na.rm=T)
    dims[[2]][["high"]] = dim2 - sd(dim2, na.rm=T)
    dims[[2]][["low"]] = dim2 + sd(dim2, na.rm=T)
    dims[[3]][["high"]] = dim3 - sd(dim3, na.rm=T)
    dims[[3]][["low"]] = dim3 + sd(dim3, na.rm=T)

    spec_dim1_high = dv ~ dim3 * dim2 * dims[[1]][["high"]]
    spec_dim1_low = dv ~ dim3 * dim2 * dims[[1]][["low"]]
    summary_dim1_high = summary(glm(spec_dim1_high, family=family))
    summary_dim1_low = summary(glm(spec_dim1_low, family=family))

    for (level in c("low", "high")) {
      spec = dv ~ dim3 * dim2 * dims[[1]][[level]]
      summary_coefficients = summary(glm(spec, family=family))$coefficients
      if (summary_coefficients[5,4] < 0.05) {
        cat(paste("with", dim1_name, level, "p=", summary_coefficients[5,4], "\n"))

        for (sublevel in c("low", "high")) {
          spec_sublevel = dv ~ dims[[3]][[sublevel]] * dim2 * dims[[1]][[level]]
          summary_sublevel_coefficients = summary(glm(spec_sublevel, family=family))$coefficients

          if (summary_sublevel_coefficients[[3, 4]] < 0.05) {
            cat(paste("with", dim1_name, level, "and", dim3_name, sublevel, "p=",
                    summary_sublevel_coefficients[[3, 4]], "\n"))
          }
        }

        spec = intercepts = stderr = list()

        spec[[1]] = dv ~ dims[[3]][["high"]] * dims[[2]][["low"]] * dims[[1]][[level]]
        spec[[2]] = dv ~ dims[[3]][["low"]] * dims[[2]][["low"]] * dims[[1]][[level]]
        spec[[3]] = dv ~ dims[[3]][["high"]] * dims[[2]][["high"]] * dims[[1]][[level]]
        spec[[4]] = dv ~ dims[[3]][["low"]] * dims[[2]][["high"]] * dims[[1]][[level]]

        for (i in 1:4) {
          fit = summary(glm(spec[[i]], family=family))
          intercepts[[i]] = fit$coefficients[1,1]
          stderr[[i]] = fit$coefficients[1,2]
        }

        display <- merge(display, data.frame(
          dim1 = rep(level, 4),
          dim2 = c('high', 'high', 'low', 'low'),
          dim3 = c('high', 'low', 'high', 'low'),
          means = as.numeric(intercepts),
          ci = as.numeric(stderr)
        ), all=T)
      }
    }

    display$dim1 <- factor(display$dim1, levels=c('low', 'high'))
    display$dim2 <- factor(display$dim2, levels=c('low', 'high'))
    display$dim3 <- factor(display$dim3, levels=c('low', 'high'))
    display = display[1:8,]

    ggplot(display, aes(x=dim3, y=means, group=dim2)) +
      facet_grid(. ~ dim1) +
      geom_line(aes(linetype=dim2), size=1) +
      geom_point(size=3, fill="white") +
      scale_x_discrete(labels=c("low", "high")) +
      scale_linetype(labels=c(paste("high\n", dim2_name), paste("low\n", dim2_name))) +
      coord_cartesian(xlim=c(0.8, 2.2)) +
      xlab(dim3_name) +
      ylab(dv_name) +
      geom_errorbar(aes(ymin=means-ci, ymax=means+ci), alpha=0.3, width=.05) +
      opts(title = paste(dv_name, "~", dim1_name, "*\n", dim2_name, "*", dim3_name, sep=" ")) +
      opts(plot.title = theme_text(size=10)) +
      opts(legend.title=theme_blank()) +
      opts(legend.position="bottom") +
      opts(legend.text = theme_text(size = 11)) +
      opts(axis.text.x = theme_text(size = 14)) +
      opts(axis.text.y = theme_text(size = 14))
  })
}
