\name{lonlat2rotpol}
\alias{lonlat2rotpol}
\alias{rotpol2lonlat}
\title{ Coordinate transformation between lon/lat and rotated lon/lat }
\description{
  \code{lonlat2rotpol} calculates for a point on the sphere (specified by 
  geographical longitude and latitude) the coordinates (longitude and 
  latitude) in a spherical coordinate system with a rotated pole. 
  \code{rotpol2lonlat} does the reverse transformation.
}
\usage{
lonlat2rotpol(lon, lat, plon, plat)
rotpol2lonlat(rlon, rlat, plon, plat)
}
\arguments{
  \item{lon}{ A numeric vector with geographic longitudes to be converted. }
  \item{lat}{ A numeric vector with geographic latitudes to be converted. }
  \item{rlon}{ A numeric vector with rotated longitudes to be converted. }
  \item{rlat}{ A numeric vector with rotated latitudes to be converted. }
  \item{plon}{ The geographic longitude of the rotated pole. }
  \item{plat}{ The geographic latitude of the rotated pole.}
}
\details{
  The coordinate transformation defines the 0 degree longitude as the
  longitude of the true north pole. 
}
\value{
  A two column matrix with column 1 beeing longitude and column 2 
  beeing latitude eighter in rotated coordinates (\code{lonlat2rotpol})
  or in normal geographic coordinates (\code{rotpol2lonlat}). 
}
\references{ Implementated after FORTRAN code by Detlev Majewski
     (DWD, Offenbach) and David Bresch (formerly IACETH, Zurich) }
\author{ Christoph Frei and Jan Kleinn }
\note{  }
\seealso{ \code{\link{geocors.trafo}} }
\examples{
lon <- c(-3.1245, 3.563)
lat <- c(-11.312,-2.4995)
hallo1 <- lonlat2rotpol(lon,lat,plon=-170.0,plat=32.5)
hallo2 <- rotpol2lonlat(rlon=hallo1[,1],rlat=hallo1[,2],plon=-170.0,plat=32.5)

}
\keyword{ spatial }% at least one, from doc/KEYWORDS
