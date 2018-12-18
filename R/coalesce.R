coalesce <- function(...) {
    Reduce(
        function(x1,x2) {
            ifelse(is.na(x1),x2,x1)
        }, list(...)
    )
}
