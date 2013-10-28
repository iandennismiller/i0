\name{target.formula}
\alias{target.formula}
\title{Create a target object.}
\usage{
  target.formula(formula, data = list(),
    family = "gaussian", mlm = FALSE, ...)
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
data = data.frame(y = c(0, 1, 2), x1 = c(2, 4, 6), x2 = c(3, 6, 9))
t = target(y ~ x1 * x2, data=data)
t$point
}
\keyword{character}

