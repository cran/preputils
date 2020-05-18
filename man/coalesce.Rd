\name{coalesce}
\encoding{latin1}
\alias{coalesce}
\title{select 1st existing value of several columns}
\description{Inspired by the respectively named sql function.
    In a list of vectors with equal length the function for each
    of the observations selects the first value that is not NA,
    in the order provided}
\usage{coalesce(...)}
\arguments{
    \item{...}{vectors holding the values, sepearated by ","
    (commonly columns of data.frame)}
}

\details{
    Data are transformed according to \deqn{b=\frac{2^M}{2^M+1}}
}

\value{
    A named vector holding the supplemented values
}

\examples{
    a1 = c(1,NA,NA,NA)
    a2 = c(2,2,NA,NA)
    a3 = c(NA,3,3,NA)
    cbind(a1,a2,a3,suppl=coalesce(a1,a2,a3))
}
