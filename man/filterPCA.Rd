\name{filterpca}
\encoding{latin1}
\alias{filterpca}
\title{Filter data set using PCA}
\description{Noise removal in data set by means of using principal component analysis. Optionally calculate distances (reconstruction error and Mahalanobis distance
    }
\usage{filterpca(x,npc=NULL,pcs=NULL,scale.=F,
	method=c("k","t"),resulttype=c("p","d","b"),lambda=NULL)}
\arguments{
    \item{x}{data set}
    \item{npc}{Number of leading principal components to be used for reconstruction of data set after filtering (positive integer) or number of last components to be skipped (negative integer).}
    \item{pcs}{Vector of integers providing column numbers of components to be included for reconstruction (positive numbers) or components to be skipped (negative numbers). In case of mixed signs negative numbers are ignored.}
    \item{scale.}{should values be scaled to unit variance before PCA?}
    \item{method}{One of either "k" or "t", with following meaning: "k": No further filtering except from ignoring some components when projecting back into original space; "t": Additionally threshold data by setting all value with absolute value below lambda to 0}
    \item{resulttype}{Type of resulting value, either matrix of projected values (p), distances (d) or a list containing both (b)}
    \item{lambda}{cutoff to be used for thresholding data. Lambda = NULL instructs to use a predefined value of 5\% of the mean deviation}
}

\details{
    The function performs PCA on provided data set. Noise is removed by reconstructing original values either on only a subset of extracted PCs, thresholding PC-scores (setting all values with absolute value below provided cutoff to 0) or a combination of both.
}

\value{
    Depending on requested resulttype:
    \item{p}{Matrix with original observations projected back onto original attribute space after filtering}
    \item{d}{Data frame with Mahalanobis distance of observations calculated only on subset of requested PCs and with reconstruction error}
    \item{b}{List containing both values mentioned above}    
}

\examples{
    a = iris[-5]
    b0 = filterpca(a,npc=4,res="b")
    b1 = filterpca(a,npc=3,res="b")
    b2 = filterpca(a,npc=2,res="b")
    pairs(b0,pch=20,col=iris$Species)
    pairs(b1,pch=20,col=iris$Species)
    pairs(b2,pch=20,col=iris$Species)
}
