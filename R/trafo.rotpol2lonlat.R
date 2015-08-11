`trafo.rotpol2lonlat` <-
function (xy,from.pars=list(plon=0,plat=90),to.pars=NULL) {
      rotpol2lonlat(rlon=xy[,1],rlat=xy[,2],
                    plon=from.pars$plon,plat=from.pars$plat)
}

