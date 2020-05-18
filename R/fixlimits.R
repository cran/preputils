fixlimits <- function(x) {
    a <- min(x[x>0 & x<1], na.rm = T)
    b <- max(x[x>0 & x<1], na.rm = T)
    x[x<=0] <- a*.5
    x[x>=1] <- (1+b)*.5
    return(x)
}
