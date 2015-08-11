\name{trafo.lonlat2rotpol}
\alias{trafo.lonlat2rotpol}
\alias{trafo.rotpol2lonlat}
\alias{trafo.rotpol2rotpol}
\alias{trafo.swisscors2lonlat}
\alias{trafo.lonlat2swisscors}
\alias{trafo.lonlat2lonlat}
\alias{trafo.lonlat2etrs.laea}
\alias{trafo.etrs.laea2lonlat}
\alias{trafo.etrs.laea2swisscors}
\alias{trafo.swisscors2etrs.laea}
\alias{trafo.etrs.laea2rotpol}
\alias{trafo.rotpol2etrs.laea}
\alias{trafo.etrs.austria2lonlat}
\alias{trafo.etrs.austria2etrs.utm32}
\alias{trafo.lonlat2etrs.austria}
\alias{trafo.etrs.utm322lonlat}
\alias{trafo.etrs.utm322etrs.austria}
\alias{trafo.lonlat2etrs.utm32}
\title{ Coordinate transformation }
\description{
  Functions used by \code{geocors.trafo} for coordinate 
  transformation. Usually these should not be invoked by the user directly.
  Use \code{geocors.trafo} instead.
}
\usage{
trafo.lonlat2rotpol(xy, from.pars = NULL, to.pars = list(plon = 0, plat = 90))

trafo.rotpol2lonlat(xy, from.pars = list(plon = 0, plat = 90), to.pars = NULL)

trafo.rotpol2rotpol(xy, from.pars = list(plon = 0, plat = 90), 
                    to.pars = list(plon = 0, plat = 90))

trafo.swisscors2lonlat(xy, from.pars = NULL, to.pars = NULL)

trafo.lonlat2swisscors(xy, from.pars = NULL, to.pars = NULL)

trafo.lonlat2lonlat(xy, from.pars = NULL, to.pars = NULL)

trafo.lonlat2etrs.laea(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.laea2lonlat(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.laea2swisscors(xy, from.pars = NULL, to.pars = NULL)

trafo.swisscors2etrs.laea(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.laea2rotpol(xy, from.pars = NULL, 
                       to.pars = list(plon = 0, plat = 90))

trafo.rotpol2etrs.laea(xy, from.pars = list(plon = 0, plat = 90), 
                       to.pars = NULL)

trafo.lonlat2etrs.austria(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.austria2lonlat(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.austria2etrs.utm32(xy, from.pars = NULL, to.pars = NULL)

trafo.lonlat2etrs.utm32(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.utm322lonlat(xy, from.pars = NULL, to.pars = NULL)

trafo.etrs.utm322etrs.austria(xy, from.pars = NULL, to.pars = NULL)

}
\arguments{
  \item{xy}{ A two column matrix. }
  \item{from.pars}{ List with parameters. }
  \item{to.pars}{ List with parameters. }
}
\details{
  
}
\value{
  A two column matrix.
}
\references{  }
\author{ Christoph Frei }
\note{ 
}
\seealso{ \code{\link{geocors.trafo}} }
\examples{
}
\keyword{ spatial }
