    vifx <- function(x) {
        data1 <- as.data.frame(x)
        data1 <- data1[sapply(data1,is.numeric)]
        r2 <- sapply(names(data1),(function(y) {
            m1 <- bquote(lm(.(as.name(y))~.,data=data1))
            result <- summary(eval(m1))
            return(result$r.squared)
        }))
        return(1 / (1 - r2))
    }
