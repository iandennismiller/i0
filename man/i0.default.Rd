\name{i0.default}
\alias{i0.default}
\title{Create an i0 object.}
\usage{
  i0.default(x, y, ...)
}
\arguments{
  \item{x}{input model function specification y data frame}
}
\value{
  i0 object
}
\description{
  Create an i0 object.
}
\examples{
data(cats, package="MASS")
i0(Hwt~Bwt*Sex, data=cats)
}
\keyword{character}

