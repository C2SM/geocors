`trafo.etrs.laea2swisscors` <-
function (xy,from.pars=NULL,to.pars=NULL) {
      res <- trafo.etrs.laea2lonlat(xy=xy)
      res <- trafo.lonlat2swisscors(xy=res)
      res
}
