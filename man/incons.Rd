\name{incons}
\encoding{latin1}
\alias{incons}
\title{Detect inconsistencies in 1:1 mapping}
\description{In properly normalized data bases, no inconsistencies
    should be present. In real world data however ID mappings or
    data base key candidates are repeated over and over across
    observations, especially in mult centric studies with basic
    research data. incons tries to detect and flag these mapping
    discrepanices}
\usage{incons(x, y, printproblems=FALSE)}
\arguments{
    \item{x,y}{vectors of equal length, holding the mapping values,
    sepearated by ","}
    \item{printproblems}{Should a table of found problems be printed
    in addition to the returned flag?}
}

\details{
    incons assumes a 1:1 mapping between provided variables, as is
    commonly the case for example in ID translation steps
}

\value{
    A named vector indicating whether ambiguous mapping does occur (TRUE) 
    or mapping is clean (FALSE)
}

\examples{
    id1 = c(1,2,2,3,4)
    id2 = c("a","b","c","d","d")
    ambiguous <- incons(id1,id2,print=TRUE)
    data.frame(id1,id2,ambiguous)
}
