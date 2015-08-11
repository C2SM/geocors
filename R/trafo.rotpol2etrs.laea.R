`trafo.rotpol2etrs.laea` <-
function (xy,from.pars=list(plon=0,plat=90),to.pars=NULL) {
      res <- trafo.rotpol2lonlat(xy=xy,from.pars=from.pars)
      res <- trafo.lonlat2etrs.laea(xy=res)
      res
}
