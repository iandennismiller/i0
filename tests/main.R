library(devtools)
dev_mode()
library('i0')

data(cats, package="MASS")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw

mod1 <- i0(x, y)
mod1

summary(mod1)
summary(i0(Hwt~Bwt*Sex, data=cats))
