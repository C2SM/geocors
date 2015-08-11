\name{lonlat2etrs.laea}
\alias{lonlat2etrs.laea}
\alias{etrs.laea2lonlat}
\title{ Convert between ETRS-LAEA coordinates and longitude/latitude }
\description{
  \code{lonlat2etrs.laea} converts points specified in geodetic longitude
  and latitude into coordinates of the ETRS89 Lambert Azimuthal Equal Area
  Coordinate Reference System (ETRS-LAEA) - a metric coordinate system
  for Europe. 

  \code{etrs.laea2lonlat} converts in the reverse diration.

}
\usage{
lonlat2etrs.laea(lon, lat)

etrs.laea2lonlat(X, Y)
}
\arguments{
  \item{X}{ A vector of easting values in ETRS-LAEA coordinates 
         (in meters) }
  \item{Y}{ A vector of northing values in ETRS-LAEA coordinates 
         (in meters) }
  \item{lon}{ A vector of geographical longitudes  
         (in degrees) }
  \item{lat}{ A vector of geographical latitudes  
         (in degrees) }
}
\details{
  
}
\value{
  \code{lonlat2etrs.laea}: A two column matrix with the geographical longitude 
       (in degrees) in the first column and the geographical latitude
       in the second column.

  \code{etrs.laea2lonlat}: A two column matrix with the easting 
       (in meters) in the first column and the northing
       in the second column.
}
\references{ 
  The ETRS89-LAEA coordinate system is described in Annoni et al. 2003.

  http://www.ec-gis.org/sdi/publist/pdfs/annoni-etal2003eur.pdf

  Conversion formuli are to be found on p. 123-124. Unfortunately there is a
  bug in the definition of B. The correct formuli can be found
  in OGP 2010, pages 73-75:

  http://www.epsg.org/guides/docs/G7-2.pdf

}
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{geocors.trafo}} }
\examples{
lonlat2etrs.laea(lon=5,lat=50)
# this should give  X(easting) = 3962799, Y(northing) = 2999719

lonlat2etrs.laea(lon=c(8,8,9),lat=c(47,48,49))

etrs.laea2lonlat(X=3962799,Y=2999719)
# this should give  longitude = 4.99994, latitude = 50.000001

}
\keyword{ spatial }% at least one, from doc/KEYWORDS
