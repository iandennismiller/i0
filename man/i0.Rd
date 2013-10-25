\name{i0}
\alias{i0}
\title{Create an i0 object.}
\usage{
  i0(x, ...)
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

