incons <- function(x, y, printproblems=FALSE) {
    `:=` <- function(...) NULL
    nx <- ny <- conflict <- NULL
    if (length(x)!=length(y)) stop("Arguments have different length")
    dt1 <- data.table::data.table(x,y)
    complete <- unique(dt1)
    complete <- complete[complete.cases(complete)]
    complete[, nx:=.N, by=x]
    complete[, ny:=.N, by=y]
    ambiguous <- complete[nx+ny>2]
    dtout <- dt1
    dtout[, conflict := FALSE]
    dtout[(x %in% ambiguous$x) | (y %in% ambiguous$y), conflict := TRUE]
    if (printproblems) {
        output <- dtout[conflict==TRUE, list(x,y)][order(x,y)]
        print(unique(output[complete.cases(output)]))
    }
    copy(dtout$conflict)
}
