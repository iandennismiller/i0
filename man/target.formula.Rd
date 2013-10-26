\name{target.formula}
\alias{target.formula}
\title{Create an target object.}
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
  Create an target object.
}
\examples{
data(cats, package="MASS")
summary(target(Hwt~Bwt*Sex, data=cats))
}
\keyword{character}

