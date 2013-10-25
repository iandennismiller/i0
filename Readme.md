# i0

# installation

    install.packages("devtools")
    library("devtools")
    install_github("devtools")
    install_github("iandennismiller/i0")

# usage

    library('i0')

    data(cats, package="MASS")
    x = cbind(Const=1, Bwt=cats$Bwt)
    y = cats$Hw

    my_model = i0(x, y)
    my_model

    summary(my_model)
    summary(i0(Hwt~Bwt*Sex, data=cats))

# development

    install.packages("roxygen2")
