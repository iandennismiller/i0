# i0

## installation

In the console of R or RStudio, type the following.

```R
install.packages("devtools")
install_github("i0", "iandennismiller")
```

The `devtools` package is required to install `i0` so the commands above install that package first, then install `i0` second.

## usage

```R
library('i0')

# load archival data
data(cats, package="MASS")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw

# create the i0 targeted object
my_model = target(x, y)
my_model

# investigate summary of results
summary(my_model)
summary(target(Hwt~Bwt*Sex, data=cats))
```

# for developers

## source code

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check

## Reference

### R packaging

- http://cran.r-project.org/doc/manuals/R-exts.html
- http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
- http://adv-r.had.co.nz/Package-basics.html

### devtools

- http://cran.r-project.org/web/packages/devtools/devtools.pdf
- https://github.com/hadley/devtools

### roxygen2

- http://cran.r-project.org/web/packages/roxygen2/index.html
- http://www.rstudio.com/ide/docs/packages/documentation

### testthat

- http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

### CRAN

- http://cran.r-project.org/web/packages/policies.html
- http://cran.r-project.org/submit.html

