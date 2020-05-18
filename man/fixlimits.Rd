\name{fixlimits}
\encoding{latin1}
\alias{fixlimits}
\title{Fix extremes for logit transformation}
\description{Change extreme values in proprtional data
    prior to logit transformation}
\usage{fixlimits(x)}
\arguments{
    \item{x}{name of vector to adjust}
}

\details{
    The function assumes a data range of 0<=x<=1. Data outside this
    range are regarded as measurement errors and recoded to NA.
    In order to avoid generating missings during logit transformation
    values >=1 and <=0 respectively are shifted to lie within the
    range (0,1) excluding the borders themselves by recoding them to
    the mean of the respective border and and the most extreme
    nearest neighbour.
}

\value{
    vector of same length as x with adjusted values
}

\examples{
    fixlimits(0:5/5)
}
