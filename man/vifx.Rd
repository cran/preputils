\name{vifx}
\encoding{latin1}
\alias{vifx}
\title{Compute Variance inflation factor}
\description{Calculate variance inflation factors (VIF) for all numeric variables contained in data set}
\usage{vifx(x)}
\arguments{
    \item{x}{name of data frame for which the VIFs should be computed}
}

\details{
    The function reads in the object named in x, builds a linear model for each of the contained variables in the data set, regressing the selected variable on all other numeric variables contained in the data set. 
    
    The multiple R-squared is computed and transformed to VIF using following formula:
    \eqn{VIF_i=\frac{1}{1-R_i^2}}
    
}

\value{
    A named vector with names given by the contained numeric variables and values by the computed respective VIFs
}

\examples{
    vifx(iris)
}
