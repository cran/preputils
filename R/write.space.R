write.tab <- function(data, filename, quote=FALSE, row.names=FALSE, ...) {
    write.table(x=data, file=filename, quote=quote, row.names=FALSE, ...)
}
