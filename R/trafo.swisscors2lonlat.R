`trafo.swisscors2lonlat` <-
function(xy,from.pars=NULL,to.pars=NULL) {
        res <- swisscors2lonlat(chx=xy[,1],chy=xy[,2])
        if (!("matrix" %in% class(res))) {
           if (length(res) != 2) {
              stop("length of data object expected to be 2")
           }
           res <- matrix(res,nrow=1)
        }
        res
}

