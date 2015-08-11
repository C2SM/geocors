`trafo.swisscors2etrs.laea` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- trafo.swisscors2lonlat(xy=xy)
      res <- trafo.lonlat2etrs.laea(xy=res)
      res
}
