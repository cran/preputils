write.tab <- function(data, filename, sep="\t", quote=FALSE, row.names=FALSE, ...) {
    write.table(x=data, file=filename, sep, quote, row.names, ...)
}
