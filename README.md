# i0

## installation

In the console of R or RStudio, type the following.

```R
require("devtools")
install_github("i0", "iandennismiller")
```

The `devtools` package is required to install `i0` so if the command above does not work, then try installing `devtools` first:

```R
install.packages("devtools")
install_github("i0", "iandennismiller")
```

## usage

### minimal example (gaussian)

```R
# load the i0 library
require('i0')

# we will use this simulation data (i.e. archival data)
data(simulated_gaussian_centered)

# create a zero-targeted model using simulation data
zt = target(y ~ x1 + x2 + x1 * x2, data=simulated_gaussian_centered)

# explore the results at +/- 1 standard deviation
plot(zt)
```

![gaussian results](http://iandennismiller.github.io/i0/gaussian_sim.png)

### logistic regression example

```R
data(baylor_religion)
formula = religious.homophilly ~ centered.church.friends*centered.income
logistic.glm <- target(formula, family="binomial", data=baylor_religion)
summary(logistic.glm)
```

### plotting scaled and raw values

```R
plot(logistic.glm)
plot(logistic.glm, raw=T)
```

## support

Please visit the [issue tracker](https://github.com/iandennismiller/i0/issues) on github to report a problem.

## for developers

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check
