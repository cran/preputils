\name{write.data}
\encoding{latin1}
\alias{write.tab}
\alias{write.space}
\title{Create text data files using convenient defaults}
\description{Several presets are provided for creating text data files.
    Functions are based on write.table, with some predefined extras
    to save time when writing data sets to clear text files}
\usage{write.tab(data, filename, sep="\\t", quote=FALSE, row.names=FALSE, ...)}
\arguments{
    \item{data}{name of object to be saved}
    \item{filename}{File name}
    \item{sep}{Column separator, see details}
    \item{quote}{Should text data be quoted? Default FALSE}
    \item{row.names}{Whether rownames whould be included in output,
        default=FALSE}
    \item{...}{Further arguments passed on to write.table()}
}

\details{
   Both of the named functions just use the filename as 2nd positional argument and call write.table(). Difference between both is that write.tab has predefined the column separator as "\\t", while write.space uses the write.table default " ".
   
}


\examples{
    \dontrun{
        write.tab(iris,"~/iris_tab.txt")
        write.space(iris,"~/iris_space.txt")
    }
}
