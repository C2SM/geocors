`trafo.lonlat2rotpol` <-
function (xy,from.pars=NULL,to.pars=list(plon=0,plat=90)) {
      lonlat2rotpol(lon=xy[,1],lat=xy[,2],plon=to.pars$plon,plat=to.pars$plat)
}
