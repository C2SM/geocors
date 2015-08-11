`trafo.etrs.austria2lonlat` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- etrs.austria2lonlat(X=xy[,1],Y=xy[,2])
      if (!("matrix" %in% class(res))) {
           if (length(res) != 2) {
              stop("length of data object expected to be 2")
           }
           res <- matrix(res,nrow=1)
      }
      res
}
