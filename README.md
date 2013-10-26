# i0

## installation

    install.packages("devtools")
    install_github("i0", "iandennismiller")

## usage

    library('i0')

    # load archival data
    data(cats, package="MASS")
    x = cbind(Const=1, Bwt=cats$Bwt)
    y = cats$Hw

    # create the i0 object
    my_model = i0(x, y)
    my_model

    # investigate summary of results
    summary(my_model)
    summary(i0(Hwt~Bwt*Sex, data=cats))

## development

    git clone http://www.github.com/iandennismiller/i0
    cd i0
    make test
    make check

# Reference

## R packaging

- http://cran.r-project.org/doc/manuals/R-exts.html
- http://cran.r-project.org/doc/contrib/Leisch-CreatingPackages.pdf
- http://adv-r.had.co.nz/Package-basics.html

## devtools

- http://cran.r-project.org/web/packages/devtools/devtools.pdf
- https://github.com/hadley/devtools

## roxygen2

- http://cran.r-project.org/web/packages/roxygen2/index.html
- http://www.rstudio.com/ide/docs/packages/documentation

## testthat

- http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

## CRAN

- http://cran.r-project.org/web/packages/policies.html
- http://cran.r-project.org/submit.html
