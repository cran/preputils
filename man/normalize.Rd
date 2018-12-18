\name{normalize}
\encoding{latin1}
\alias{normalize}
\title{Normalize numeric variable to range(0,1)}
\description{Changes range of numeric variables to have min=0 and max=1}
\usage{normalize(x)}
\arguments{
    \item{x}{name of object to normalize}
}

\details{
    The function changes the range of the named numeric vector to finally have min(x)=0 and max(x)=1.     
}

\value{
    vector of same length as x with normalized values
}

\examples{
    normalize(1:5)
}
