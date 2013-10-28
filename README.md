# i0

*i0* (pronounced "i-zero") provides a simple interface for exploring interactions in linear models. This R package is an implementation of the methods in Page-Gould and Miller (submitted), which extends Aiken and West (1991).

## usage

```R
# load the i0 library
library(i0)

# the following dataset comes with the i0 package
data(candy_example)

# create a zero-targeted model 'candy ~ age + height + age * height' using the dataset
zt = target(candy ~ age + height + age * height, data=candy_example)

# explore the results at +/- 1 standard deviation like Aiken and West (1991)
plot(zt)
```

![gaussian results](http://iandennismiller.github.io/i0/gaussian_sim.png)

## installation

The *i0* R package is easily installed using R or RStudio. In the R console, load *devtools* and install *i0* from the project website:

```R
library(devtools)
install_github("i0", "iandennismiller")
```

If you receive the message `Error in library(devtools) : there is no package called devtools’`, you must first install *devtools* (see instructions below) then try again.

```R
install.packages("devtools")
```

## documentation

- [Examples](https://github.com/iandennismiller/i0/wiki/Examples)
- [Project Website](http://iandennismiller.github.io/i0)
- [Source Code](https://github.com/iandennismiller/i0)

## support

Please visit the [issue tracker](https://github.com/iandennismiller/i0/issues) on github to report a problem.

## references

Aiken, L. S., & West, S. G. (1991). Multiple regression: Testing and interpreting interactions. Sage Publications, Inc.

Page-Gould, E., & Miller, I. D. (submitted). Zeroing in on the intercept: Estimating marginal means and standard errors with linear models.

## for developers

Browse the [Source Code](https://github.com/iandennismiller/i0) or check out the git repo:

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check
