`trafo.etrs.laea2rotpol` <-
function (xy,from.pars=NULL,to.pars=list(plon=0,plat=90)) {
      res <- trafo.etrs.laea2lonlat(xy=xy)
      res <- trafo.lonlat2rotpol(xy=res,to.pars=to.pars)
      res
}
