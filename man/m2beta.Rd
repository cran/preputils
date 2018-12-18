\name{m2beta}
\encoding{latin1}
\alias{m2beta}
\title{Convert logit transformed M-Values of proportional data
    back to original 0/1 range}
\description{Despite conducting analysis of proportional data in M space,
    for publication figures the estimated values are commonly shown
    in the original space (range between 0 and 1). This function
    provides backscaling of the M values to original space by
    inverting the logit transformation done by beta2m()}
\usage{m2beta(M)}
\arguments{
    \item{M}{vector or matrix holding the original data}
}

\details{
    Data are transformed according to \deqn{b=\frac{2^M}{2^M+1}}
}

\value{
    A named vector/matrix with same dimensions as M and transformed values
}

\examples{
    b = 1:99 / 100
    M = beta2m(b)
    plot(b,m2beta(M))
    print(all.equal(b, m2beta(M)))
}
