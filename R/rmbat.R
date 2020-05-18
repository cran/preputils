rmbat = function(x, batches) {
    stopifnot(ncol(x) == length(batches))
    val1 <- matrix(data = as.numeric(NA), nrow(x), ncol(x))
    rownames(val1) <- rownames(x)
    colnames(val1) <- colnames(x)
    meanall <- rowMeans(x, na.rm = TRUE)
    for (batch in unique(batches)) {
        cols <- which(batches == batch)
        if (length(cols) > 1) {
            meanbatch <- rowMeans(x[, cols, drop = FALSE], na.rm = TRUE)
            val1[,cols] <- sweep(x[, cols, drop = FALSE], 1, meanbatch-meanall)
        } else {
            val1[,cols] <- meanall
        }
    }
    val1[is.na(x)] <- NA
    val1[, is.na(batches)] <- NA
    return(val1)
}
