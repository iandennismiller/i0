# i0

# installation

    install.packages("devtools")
    library("devtools")
    install_github("devtools")
    install_github("iandennismiller/i0")

# then inside R

    library('i0')
    data(cats, package="MASS")
    x = cbind(Const=1, Bwt=cats$Bwt)
    y = cats$Hw
    mod1 <- i0(x, y)
    mod1
    summary(mod1)
    summary(i0(Hwt~Bwt*Sex, data=cats))

# development

    install.packages("roxygen2")
    