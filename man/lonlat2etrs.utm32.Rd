\name{lonlat2etrs.utm32}
\alias{lonlat2etrs.utm32}
\alias{etrs.utm322lonlat}
\title{ Convert between Austrian ETRS89 UTM zone 32N and longitude/latitude }
\description{
  \code{lonlat2etrs.utm32} converts points specified in geodetic longitude
  and latitude into the ETRS89 UTM32 coordinate system in Austria - a metric 
  coordinate system (EPSG 25832).

  \code{etrs.utm322lonlat} converts in the reverse diration.

}
\usage{
lonlat2etrs.utm32(lon, lat)

etrs.utm322lonlat(X, Y)
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
  \code{lonlat2etrs.utm32}: A two column matrix with the geographical longitude 
       (in degrees) in the first column and the geographical latitude
       in the second column.

  \code{etrs.utm322lonlat}: A two column matrix with the easting 
       (in meters) in the first column and the northing
       in the second column.
}
\references{

  Conversion formuli were implemented after:
  http://www.epsg.org/guides/docs/G7-2.pdf
  pp. 47-48, version July 2012, JHS formulas 

  Tests and test-dataset from:
  http://www.geoland.at/gps_trans.htm

}
\author{ Johann Hiebl }
\note{  }

\seealso{ \code{\link{geocors.trafo}} }
\examples{
data(austria.cities)
qq <- lonlat2etrs.utm32(lon=austria.cities[,"lon"],lat=austria.cities[,"lat"])
print(qq)

etrs.utm322lonlat(X=qq[,"X"],Y=qq[,"Y"])

}
\keyword{ spatial }% at least one, from doc/KEYWORDS
