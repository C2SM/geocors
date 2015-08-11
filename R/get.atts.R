`get.atts` <-
function(from,which=NULL,
         which.not=c("dim","dimnames","names","row.names","class")) {
    nn <- names(attributes(from))
    if (is.null(nn)) return(list())
    if (is.null(which)) {
        which <- nn[!(nn %in% which.not)]
    }
    if (is.null(which)) return(list())
    attributes(from)[which]
}

