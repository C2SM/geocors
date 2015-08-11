\name{geocors.trafo}
\alias{geocors.trafo}
\title{ Transform between coordinate systems }
\description{
  Transform data objects between various coordinate systems. Several input
  objects representing points on the earth surface in different contexts
  can be handled (vectors, matrices, data frames, lists, arrays). The output 
  is of the same class like the input object and has attributes representing
  the type and parameters of the coordinate system. Row names are appropriately
  transferred when the input is a matrix or data frame.
}
\usage{
geocors.trafo(x, y = NULL, from.type, from.pars, to.type, to.pars)
}
\arguments{
  \item{x}{ A numeric vector representing the x coordinates of the points
      to be transformed. See details for other input classes. }
  \item{y}{ A numeric vector representing the y coordinates of the points
      to be transformed. }
  \item{from.type}{ A character string with the name of the input coordinate
      system, i.e. the system in which x and y are defined. Alternatively
      the type can be specified in the attributes section of argument
      \code{x}. (See details for available coordinate types.) }
  \item{from.pars}{ A list with parameters of the input coordinate system.
      Not all coordinate systems require additional parameters 
      (e.g. \code{from.type="lonlat"}), in which case the argument can
      be left missing, 
      but some do (e.g. \code{from.type="rotpol"}). Alternatively
      the parameters can be specified in the attributes section of argument
      \code{x}. (See details.) }
  \item{to.type}{ A character string with the name of the coordinate
      system into which the input (\code{x and y} shall be transformed. }
  \item{to.pars}{ A list with parameters of the output (i.e. target-) 
      coordinate system. This argument can be left missing if no additional
      parameters are required by the target coordinate system. }
}
\details{
  ** Available coordinate system types, coordinate names and parameters **

  \code{"lonlat"} : regular longitude latitude grid. Coordinate values specified
     in degrees. Coordinate axis names are \code{"lon"} and \code{"lat"}.

  \code{"rotpol"} : longitude latitude grid with a rotated pole. Coordinte 
     values specified in degrees. Exact specification of a "rotpol" system
     requires additional parameters: the regular longitude of the north 
     pole \code{plon}, the regular latitude of the north pole \code{plat}.
     Transformation from/to the "rotpol"-System requires specification of
     arguments \code{from.pars} or \code{to.pars}, e.g. 
     \code{from.type=list(plon=-170.0,plat=40)}.
     Coordinate axis names for this type are \code{"rlon"} and \code{"rlat"}.
     
  \code{"swisscors"} : The Swiss coordinte system where coordinate values
     are specified in meters. (Note: The transformation from/to this system
     will not work if points are far from the territory of Switzerland. 
     Coordinate axis names for this type are \code{"chx"} and \code{"chy"}.

  \code{"etrs.laea"} : The European kilometer grid on a Lambert Azimutal
     Equal Area projection.

  \code{"etrs.austria"} : The Austrian ETRS89 Lambert coordinate system.

  \code{"etrs.utm32"} : The Austrian ETRS89 UTM32 coordinate system.
  
  ** Currently available coordinate transformations **

  "lonlat" <--> "rotpol"

  "rotpol" <--> "etrs.laea"

  "rotpol" <--> "rotpol"

  "lonlat" <--> "etrs.laea"

  "swisscors" <--> "lonlat"

  "swisscors" <--> "etrs.laea"

  "etrs.austria" <--> "lonlat"

  "etrs.utm32" <--> "lonlat"

  "etrs.utm32" <--> "etrs.austria"

  ** Data objects accepted as input **

  Instead of conventional numeric vectors for \code{x and y}, the 
  input can be specified in other data classes:
  
  \code{x} is a list with two equal length numeric vectors. List elements 
  can be named with the coordinate names of the input system type.
  If the list elements are not named, the first/second element is 
  interpreted as x/y coordinates. 

  \code{x} is a matrix with two columns. Columns 
  can be named with the coordinate names of the input system type.
  If the columns are not named, the first/second columns are interpreted as x/y
  coordinates. 

  \code{x} is a data frame with two columns. Columns 
  should be named with the coordinate names of the input system type.

  \code{x} is an array with two equal dimension layers. Layers 
  can be named (in \code{dimnames(x)[3]}) with the coordinate names of the 
  input system type. If the layers are not named, the first layer (i.e. 
  \code{x[,,1]}) is interpreted as x-coordinate, ... .

  ** Specifying coordinate systems via attributes **

  Input objects can be accompanied by attributes, specifying the type and
  parameters of the coordinate system of its contents. 
  If these attributes are there,
  no input is required for arguments \code{from.type} and \code{from.pars}. 
  However specification of \code{from.type} overwrites attributes.

  ** How to add more transformations **

  This function is fairly generic and can transform between other coordinate
  systems if this package is properly extended. For this purpose additional
  functions similar to \code{\link{trafo.lonlat2rotpol}} or 
  \code{\link{trafo.swisscors2lonlat}} are needed. These are then called
  by \code{geocors.trafo}, which does not need further adjustment.

}
\value{
  A data object with the transformed coordinates, accompanied by attributes
  for the coordinate system type and parameters. The class of the output
  depends on that for the input: If input coordinates are specified as
  vectors for \code{x} and \code{y} the output is a two element list with 
  vectors for the transformed x and y coordinates, properly named with the
  coordinate names of the target coordinate type. In all other cases the
  output class and format is the same like that for the input (see 
  details above.) Names for columns (matrix, data.frame) and for levels
  (arrays) are the target coordinate names. 

  All output objects carry an attribute \code{"grid.type"} and - if necessary -
  several attributes for the parameters of the coordinate system. This
  allows convenient treatment of objects. The following functions can deal
  with these attributes: \code{\link{get.grid.atts}},
  \code{\link{put.grid.atts}}.

}
\references{  }
\author{ Christoph Frei }
\note{ }
\seealso{ 
  Coordinate transformations can also be calculated via 
  specific transformation functions, which allow less flexible inputs and
  do not handle attributes. Examples are \code{\link{lonlat2swisscors}} and
  \code{\link{lonlat2rotpol}}.
}
\examples{

# example for trafo between longitude-latitude and swiss coordinates
geocors.trafo(x=c(8.0,9.0),y=c(47.0,48.0),
              from.type="lonlat",to.type="swisscors")
              
geocors.trafo(x=c(642000,716000),y=c(205000,317000),
              from.type="swisscors",to.type="lonlat")
              
# example for trafo between longitude-latitude and rotated pole coordinates
geocors.trafo(x=c(8.0,9.0),y=c(47.0,48.0),from.type="lonlat",
              to.type="rotpol",
              to.pars=list(plon=-170,plat=40))
              
geocors.trafo(x=c(-1.36,-0.66),y=c(-2.98,-1.99),from.type="rotpol",
              from.pars=list(plon=-170,plat=40), to.type="lonlat")
              
# example for trafo between two different rotated pole coordinates
geocors.trafo(x=c(-1.3656978,-0.6695179),y=c(-2.981740,-1.995524),
              from.type="rotpol", from.pars=list(plon=-170,plat=40),
              to.type="rotpol", to.pars=list(plon=-140,plat=60))
# check this:
geocors.trafo(x=c(8.0,9.0),y=c(47.0,48.0),from.type="lonlat",
              to.type="rotpol",
              to.pars=list(plon=-140,plat=60))


# example for matrix input
aa <- matrix(c(642000,716000,205000,317000),nrow=2,ncol=2)
rownames(aa) <- c("sample1","sample2")
geocors.trafo(x=aa,from.type="swisscors",to.type="lonlat")

# example for data.frame input
geocors.trafo(x=data.frame(chx=c(642000,716000),chy=c(205000,317000)),
              from.type="swisscors",to.type="lonlat")

# example for list input
geocors.trafo(x=list(chy=c(205000,317000),chx=c(642000,716000)),
              from.type="swisscors",to.type="lonlat")

# example for array input
aa <- array(NA,dim=c(3,2,2))
aa[,,1] <- matrix(c(7,8,9,7,8,9),nrow=3,ncol=2) 
aa[,,2] <- matrix(c(47,47,47,48,48,48),nrow=3,ncol=2) 
geocors.trafo(x=aa,from.type="lonlat",
              to.type="rotpol",to.pars=list(plon=-170,plat=40))

# examples for simple data input
geocors.trafo(x=8,y=47,from.type="lonlat",to.type="swisscors")
geocors.trafo(x=640000,y=205000,from.type="swisscors",to.type="lonlat")
geocors.trafo(x=8,y=47,from.type="lonlat",to.type="rotpol",
              to.pars=list(plon=179,plat=30))
geocors.trafo(x=6,y=-12,from.type="rotpol",to.type="lonlat",
              from.pars=list(plon=179,plat=30))


# example for array input with attributes
aa <- array(NA,dim=c(3,2,2))
aa[,,1] <- matrix(c(-2.04,-1.36,-0.66,-2.04,-1.36,-0.66),nrow=3,ncol=2) 
aa[,,2] <- matrix(c(-2.98,-2.98,-2.98,-1.99,-1.99,-1.99),nrow=3,ncol=2) 
attr(aa,"grid.type") <- "rotpol"
attr(aa,"plon") <- -170
attr(aa,"plat") <- 40
geocors.trafo(x=aa,to.type="lonlat")

# examples with data frame
data(austria.cities)
qq <- geocors.trafo(x=austria.cities,to.type="etrs.austria")
print(qq)
geocors.trafo(x=qq,to.type="lonlat")

data(austria.cities)
qq <- geocors.trafo(x=austria.cities,to.type="etrs.utm32")
print(qq)
geocors.trafo(x=qq,to.type="lonlat")
geocors.trafo(x=qq,to.type="etrs.austria")

}
\keyword{ spatial }
