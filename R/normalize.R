normalize <- function(x) {
    a <- x - min(x, na.rm=TRUE)
    return(a / max(a, na.rm=TRUE))
}
