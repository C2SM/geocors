`trafo.rotpol2rotpol` <-
function (xy,from.pars=list(plon=0,plat=90),to.pars=list(plon=0,plat=90)) {
      res <- trafo.rotpol2lonlat(xy=xy,from.pars=from.pars)
      res <- trafo.lonlat2rotpol(xy=res,to.pars=to.pars)
      res
}
