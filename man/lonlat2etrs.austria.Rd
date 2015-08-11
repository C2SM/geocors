\name{lonlat2etrs.austria}
\alias{lonlat2etrs.austria}
\alias{etrs.austria2lonlat}
\title{ Convert between Austrian ETRS89 coordinates and longitude/latitude }
\description{
  \code{lonlat2etrs.austria} converts points specified in geodetic longitude
  and latitude into the ETRS89 Lambert coordinate system in Austria - a metric 
  coordinate system (EPSG 3416).

  \code{etrs.austria2lonlat} converts in the reverse diration.

}
\usage{
lonlat2etrs.austria(lon, lat)

etrs.austria2lonlat(X, Y)
}
\arguments{
  \item{X}{ A vector of easting values in ETRS coordinates 
         (in meters) }
  \item{Y}{ A vector of northing values in ETRS coordinates 
         (in meters) }
  \item{lon}{ A vector of geographical longitudes  
         (in degrees) }
  \item{lat}{ A vector of geographical latitudes  
         (in degrees) }
}
\details{
  
}
\value{
  \code{lonlat2etrs.austria}: A two column matrix with the geographical longitude 
       (in degrees) in the first column and the geographical latitude
       in the second column.

  \code{etrs.austria2lonlat}: A two column matrix with the easting 
       (in meters) in the first column and the northing
       in the second column.
}
\references{

  Conversion formuli were implemented after:
  http://www.epsg.org/guides/docs/G7-2.pdf
  pp. 18-19, version July 2012 

  Tests and test-dataset from:
  http://www.geoland.at/gps_trans.htm

}
\author{ Johann Hiebl and Christoph Frei }
\note{  }

\seealso{ \code{\link{geocors.trafo}} }
\examples{
data(austria.cities)
qq <- lonlat2etrs.austria(lon=austria.cities[,"lon"],lat=austria.cities[,"lat"])
print(qq)

etrs.austria2lonlat(X=qq[,"X"],Y=qq[,"Y"])

}
\keyword{ spatial }% at least one, from doc/KEYWORDS
