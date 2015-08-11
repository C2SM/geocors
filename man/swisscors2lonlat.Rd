\name{swisscors2lonlat}
\alias{swisscors2lonlat}
\alias{lonlat2swisscors}
\title{ Convert between Swiss coordinates and longitude/latitude }
\description{
  \code{swisscors2lonlat} converts points specified in swiss coordinates into 
  geographical longitude and latitude.

  \code{lonlat2swisscors} converts points specified in  
  geographical longitude and latitude into swiss coordinates.

}
\usage{
swisscors2lonlat(chx, chy)

lonlat2swisscors(lon, lat)
}
\arguments{
  \item{chx}{ A vector of easting values in swiss coordinates 
         (in meters) }
  \item{chy}{ A vector of northing values in swiss coordinates 
         (in meters) }
  \item{lon}{ A vector of geographical longitudes  
         (in degrees) }
  \item{lat}{ A vector of geographical latitudes  
         (in degrees) }
}
\details{
  
}
\value{
  \code{swisscors2lonlat}: A two column matrix with the geographical longitude 
       (in degrees) in the first column and the geographical latitude
       in the second column.

  \code{lonlat2swisscors}: A two column matrix with the swiss easting 
       (in meters) in the first column and the swiss northing
       in the second column.
}
\references{ Bundesamt fuer Landestopographie, 2001: Formeln und 
      Konstanten fuer die Berechnung der Schweizerischen schiefachsigen 
      Zylinderprojektion und der Transformation zwischen 
      Koordinatensystemen. 16pp. www.swisstopo.ch }
\author{ Christoph Frei }
\note{  }

\seealso{ \code{\link{geocors.trafo}} }
\examples{
}
\keyword{ spatial }% at least one, from doc/KEYWORDS
