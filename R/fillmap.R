fillmap <- function (x, y, what = "xy", rmdup = FALSE, rmmiss = FALSE, printori = FALSE) {
    `:=` = function(...) NULL
    nx <- ny <- NULL
    if (length(x) != length(y)) 
        stop("Arguments have different length")
    dt0 <- data.table::data.table(x,y)
    dt1 <- unique(dt0)
    dt1 <- dt1[complete.cases(dt1)]
    dt1[, nx := .N, by = x]
    dt1[, ny := .N, by = y]
    dt2 <- dt1[nx + ny == 2, list(x, y)]
    dtout <- copy(dt0)
    if (printori)
        print(dt0)
    try(dtout[is.na(y) & (x %in% dt2$x), y := dt2$y[match(x,dt2$x)]],TRUE)
    try(dtout[is.na(x) & (y %in% dt2$y), x := dt2$x[match(y,dt2$y)]],TRUE)
    if (rmdup) 
        dtout <- unique(dtout)
    if (rmmiss) 
        dtout <- dtout[complete.cases(dtout)]
    if (what == "xy") 
        return(copy(dtout))
    if (what == "x") 
        return(copy(dtout$x))
    if (what == "y") 
        return(copy(dtout$y))
}
