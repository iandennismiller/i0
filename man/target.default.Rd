\name{target.default}
\alias{target.default}
\title{Create an target object.}
\usage{
  target.default(x, y, ...)
}
\arguments{
  \item{x}{input model function specification y data frame}
}
\value{
  target object
}
\description{
  Create an target object.
}
\examples{
data(cats, package="MASS")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw
mod1 <- target(x, y)
mod1
summary(mod1)
summary(target(Hwt~Bwt*Sex, data=cats))
}
\keyword{character}

