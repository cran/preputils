\name{beta2m}
\encoding{latin1}
\alias{beta2m}
\title{Convert proportional data to M-Values}
\description{Proportional data are commonly modelled using a glm
    approach with logit link function. When performing the logit
    transformation in advance separately, simple OLS methods can be
    applied.}
\usage{beta2m(b)}
\arguments{
    \item{b}{vector or matrix holding the original data}
}

\details{
    Data are transformed according to
    \ifelse{html}{\out{M=log<sub>2</sub>(<sup>b</sup>&frasl;<sub>1-b</sub>)}
    }{\eqn{M=log[2](\frac{b}{1-b})}}.
    The input data are assumed to have the range 0<b<1.
    Data outside this range will lead to missing values.
    Corner cases (data of b=0 or b=1) can be handled by use
    of fixlimits().
}

\value{
    A named vector/matrix with same dimensions as b and \ifelse{html}{\out{log<sub>2</sub>}}{\eqn{log[2]}} transformed values
}

\examples{
    a <- sapply(c(0.01,0.05,0.5,0.8,0.9),function(x) rbinom(30,100,x)/100)
    matplot(a,pch=20)
    matplot(beta2m(a),pch=20)
    matplot(a,beta2m(a),pch=20)
}
