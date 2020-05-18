\name{pcv}
\encoding{latin1}
\alias{pcv}
\title{PCA on automatically selected attributes in high dimensional data}
\description{ Conduct PCA on variables with biggest variance
    in high dimensional data matrix
    }
\usage{pcv(x, cols=5, sites=5000)}
\arguments{
    \item{x}{name of data matrix}
    \item{cols}{number of principal components to extract}
    \item{sites}{number of attributes to consider}
}

\details{
    pcv assumes data in a numeric matrix and variable major format,
    i.e. every line corresponds to to a variable,
    while the columns correspond to the individual observations.
    This is commonly the case for data in high throughput experiments
    where the number of data points per individuals is high (> 10,000),
    while the size of batches is comparably small (dozens to hundreds).
    Variables with missing values are disregarded for the selection.
    
    Use t() to transpose individual major data sets beforehand.
    
    pcv selects the attributes with the highest variance up to
    the numbers provided, but takes considerations to limit these to
    the actual size of the present data set. 
    
    This is often used as first step in high throughput measurements
    to detect global effects of known batch variables.
}

\value{
    matrix with rows corresponding to observations and columns
    to extracted components. Values denote the scores on the extracted
    components for the respective observations.
}

\examples{
    pcs <- pcv(t(iris[1:4]),cols=2)    
    cor(pcs,iris[-5])
}
