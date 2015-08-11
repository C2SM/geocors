`trafo.etrs.austria2etrs.utm32` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- trafo.etrs.austria2lonlat(xy=xy)
      res <- trafo.lonlat2etrs.utm32(xy=res)
      res
}
