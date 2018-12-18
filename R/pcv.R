pcv <- function (x, cols = 5, sites = 5000) {
    var1 <- apply(x, 1, var)
    sites <- min(abs(sites),length(var1[!is.na(var1)]))
    set1 <- order(var1, decreasing = TRUE)[1:sites]
    pcs <- prcomp(t(x[set1, ]))
    cols <- min(abs(cols),ncol(pcs$x))
    return(pcs$x[, 1:cols])
}
