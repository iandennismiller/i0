\name{target.default}
\alias{target.default}
\title{Create a target object.}
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
  Create a target object.
}
\examples{
data(cats, package="MASS")
x = cbind(Const=1, Bwt=cats$Bwt)
y = cats$Hw
mod1 <- target(x, y)
print(mod1)
}
\keyword{character}

