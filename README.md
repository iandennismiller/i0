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
