context("workflow")

data(cats, package="MASS")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw

mod1 <- target(x, y)
mod1

summary(mod1)
summary(target(Hwt~Bwt*Sex, data=cats))
