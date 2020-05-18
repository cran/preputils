\name{fillmap}
\encoding{latin1}
\alias{fillmap}
\title{Supplement missing values in mapping of data}
\description{In properly normalized data bases, 1:1 mapping should be
    complete and unique. In real world data however ID mappings or
    data base key candidates are repeated over and over across
    observations, partly with missing data in case of merged data set.
    fillmap supplements NAs in mapping variables as far as possible
    }
\usage{fillmap(x, y, what = "xy", rmdup=FALSE, rmmiss=FALSE,
    printori=FALSE)}
\arguments{
    \item{x,y}{vectors of equal length, holding the mapping values,
    sepearated by ","}
    \item{what}{data to be returned, either 1st ("x") or 2nd argument
    ("y") or a data,table, cointaining both ("xy")}
    \item{rmdup}{remove duplicates from mapping (TRUE) or return all
        rows in original order (FALSE)}
    \item{rmmiss}{remove rows, where not mapping could be found (TRUE)
        or return all rows (FALSE)}
    \item{printori}{print original variables side by side}
}

\details{
    incons assumes a 1:1 mapping between provided variables, as is
    commonly the case for example in ID translation steps. 
    
    For all cases where a proper unambiguous 1:1 matching exists. The missings
    values are filled in
}

\value{
    Vector or data.table with original mapping data, where NAs are
    filled in whith supplemented data where possible
}

\examples{
    library(data.table)
    pheno1 <- data.frame(id1=c(1,2,3,4),id2=c(11,22,NA,NA),phenodat=c(NA,NA,NA,"d"))
    pheno2 <- data.frame(id1=c(NA,NA,NA),id2=c(11,22,33),phenodat=c("a","b","c"))
    pheno3 <- data.frame(id1=c(4,3),id2=c(44,33),phenodat=c(NA,NA))
    phenoges <- rbind(rbind(pheno1,pheno2),pheno3)
    with(phenoges,fillmap(id1,phenodat))
    with(phenoges,fillmap(id1,phenodat,rmdup=TRUE))
    with(phenoges,fillmap(id1,phenodat,rmmiss=TRUE))
    with(phenoges,fillmap(id1,phenodat,rmdup=TRUE,rmmiss=TRUE))
    with(phenoges,fillmap(id2,phenodat))
    with(phenoges,fillmap(id2,phenodat,rmdup=TRUE))
    with(phenoges,fillmap(id2,phenodat,rmmiss=TRUE))
    with(phenoges,fillmap(id2,phenodat,rmdup=TRUE,rmmiss=TRUE))
    phenosupp <- with(phenoges,fillmap(id1,id2))
    names(phenosupp) <- c("id1","id2")
    phenosupp$phenodat <- fillmap(phenosupp$id1,phenoges$phenodat,what="y")
    unique(phenosupp)
}
