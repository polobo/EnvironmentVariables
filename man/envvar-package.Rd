\name{envvar-package}
\alias{envvar-package}
\alias{envvar}
\docType{package}
\title{
Envrionment Variables Helper Functions
}
\description{
Provides functions to semantically manipulate environment variables.
}
\details{
\tabular{ll}{
Package: \tab envvar\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2015-02-24\cr
License: \tab Unlimited\cr
}
Each provided function has its own instructions for use.
Pick the one that fulfills your requirements.
}
\author{
David G. Johnston

Maintainer: David G. Johnston <polobo@yahoo.com>
}
\keyword{ package }
\examples{
env_or_default(c("HOME","UNKNOWN"), c("/tmp", "DEFAULT"))
}
