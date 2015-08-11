`trafo.lonlat2etrs.austria` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- lonlat2etrs.austria(lon=xy[,1],lat=xy[,2])
      if (!("matrix" %in% class(res))) {
           if (length(res) != 2) {
              stop("length of data object expected to be 2")
           }
           res <- matrix(res,nrow=1)
      }
      res
}
