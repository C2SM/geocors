`put.atts` <-
function(to,atts=list(),
         which.not=c("dim","dimnames","names","row.names","class")) {
    if (length(atts)==0) return(to)
    nn <- names(atts)
    nn <- nn[!(nn %in% which.not)]
    if (length(nn)==0) return(to)
    for (nam in nn) {
    	attr(to,nam) <- atts[[nam]]
    }
    to
}

