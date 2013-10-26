\name{target.formula}
\alias{target.formula}
\title{Create a target object.}
\usage{
  target.formula(formula, data = list(), ...)
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
print(summary(target(Hwt~Bwt*Sex, data=cats)))
}
\keyword{character}

