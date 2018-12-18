write.tab <- function(data, filename, sep="\t", quote=FALSE, row.names=FALSE, ...) {
    write.table(data, filename, sep, quote, row.names, ...)
}
