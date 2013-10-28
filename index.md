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

### gaussian example

```R
library('i0')

# load archival data
data(simulated_gaussian_data)
d = simulated_gaussian_data

# center our variables of interest
d$x1 = as.vector(scale(d$x1, center=T))
d$x2 = as.vector(scale(d$x2, center=T))

# create a zero-targeted model
t = target(y ~ x1 + x2 + x1 * x2, data=d)

# explore the results at +/- 1 standard deviation
summary(t)
plot(t)
```

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

# for developers

## source code

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check
