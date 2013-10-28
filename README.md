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

# for developers

## source code

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check
