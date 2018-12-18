\name{rmbat}
\encoding{latin1}
\alias{rmbat}
\title{batch effect removal by mean centering and shifting}
\description{Remove known categorical batch effects from high dimensional data sets}
\usage{rmbat(x,batches)}
\arguments{
    \item{x}{name of object to be processed. This is a matrix in atribute major format (rows correspond to variables, columns to observations)}
    \item{batches}{Vector with batch identifiers for each of
        the columns in x}
}

\details{
    For each variable the mean values of all batches are shifted to the grand mean of the total sample. On case of several independent bacth effects being present in th data set, thes can either be combined in one batch variable, or the batches can be removed one at a time by chaining the processing and caling the cuntiong with each of the batch variables in turn
}

\value{
    matrix with same dimensions as x and batch effects removed
}

\examples{
    # create data set
    n_obs = 8
    n_var = 10
    predictor <- rep(0:1,n_obs*0.5)
    pure_effect <- outer(rnorm(n_var),predictor)
    error <- matrix(rnorm(n_var*n_obs),n_var,n_obs)
    batch1 <- rep(1:2,each=n_obs*0.5)
    batch2 <- rep(c(1,2,1,2),each=n_obs*0.25)
    batch_effect1 <- outer(rnorm(n_var)*2,scale(batch1))[,,1]
    batch_effect2 <- outer(rnorm(n_var)*4,scale(batch2))[,,1]
    batch_effect <- batch_effect1 + batch_effect2
    data_measured <- pure_effect + batch_effect + error
    
    zero = outer(rep(0,n_var),rep(0,n_obs))
    b1 <- rmbat(batch_effect1,batch1)
    b2 <- rmbat(batch_effect2,batch2)
    b12a <- rmbat(batch_effect1,paste(batch1,batch2))
    b12b <- batch_effect %>% rmbat(batch1) %>% rmbat(batch2)
    all.equal(b1,zero)
    all.equal(b2,zero)
    all.equal(b12a,zero)
    all.equal(b12b,zero)
}

\note{
    This function is intended for use with methods that do not inherently allow inclusion of covariates in the analysis itself, e.g. pca or heatmap. If methods are used that allow inclusion of batches in analysis like linear models, that is preferred, as the method above can otherwise greatly reduce power if batches are correlated with the effect variable
}
