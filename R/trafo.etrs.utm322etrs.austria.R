`trafo.etrs.utm322etrs.austria` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- trafo.etrs.utm322lonlat(xy=xy)
      res <- trafo.lonlat2etrs.austria(xy=res)
      res
}
