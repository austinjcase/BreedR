% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/look_function.R
\name{look}
\alias{look}
\title{look function}
\usage{
look(x)
}
\arguments{
\item{x}{REQUIRED \code{data.frame} dataframe or matrix}
}
\description{
This function allows you to pull data from the oat data base or similar
}
\examples{

df.small<-data.frame(a=rnorm(1:100), b=rnorm(1:100))
df.small
look(df.small)

df.large<-dat <- data.frame(a=rnorm(1:100), b=rnorm(1:100), c=rnorm(1:100),
                           d=rnorm(1:100), e=rnorm(1:100),f=rnorm(1:100),g=rnorm(1:100), h=rnorm(1:100),
                           i=rnorm(1:100), j=rnorm(1:100), k=rnorm(1:100), l=rnorm(1:100))
head(df.large)
look(df.large)
}
\keyword{look}
