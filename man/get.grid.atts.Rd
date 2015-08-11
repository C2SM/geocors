\name{get.grid.atts}
\alias{put.grid.atts}
\alias{get.grid.atts}
\alias{remove.grid.atts}
\alias{copy.grid.atts}
\alias{grid.att.names}
\alias{is.consistent.grid}
\title{ Handling grid attributes }
\description{
  Get / put / copy / remove geographic attributes from/to a data 
  object representing
  a geographic dataset, typically an array or a matrix representing
  a grid in space. Geographic attributes are useful in determining the spatial
  location of grid points and can be used for plotting an array/matrix
  as a spatial field, or a polygon as a line sequence.
  \code{put.grid.atts} adds/sets geographic attributes to a data object. 
  \code{get.grid.atts} reads geographic attributes and returns them as a list.
  \code{copy.grid.atts} copies geographic attributes from one data object 
  to another.
  \code{is.consistent.grid} checks consistency of geographic attributes.
  \code{grid.att.names} returns a list of geographic attributes associated 
  with a particular grid type.

  The notion "grid.atts" in the names of these functions comes from
  using them for arrays or matrices describing 2D fields (i.e. grids).
  Now the functions are more general and can be used to specify 
  geographic coordinate/location information in any type of data objects.
}
\usage{
put.grid.atts(fld, grid.type="xy", grid.cors=list(), 
              grid.pars=list(), remove=TRUE)
get.grid.atts(fld, what="all")
remove.grid.atts(fld)
copy.grid.atts(from, to)
grid.att.names(grid.type,what="all")
is.consistent.grid(g1, g2)
}
\arguments{
  \item{fld}{ A data object to/from which attributes
      shall be added/read. Typically \code{fld} is a 3D array or a 2D matrix 
      representing one or several geographic fields. }
  \item{grid.type}{ A character string representing the name of the
      geographic coordinate system. Can be one of:

      "lonlat" (for a grid in regular latitude/longitude coordinates), 

      "rotpol" (for a grid in rotated pole coordinates), 

      "swisscors" (for swiss meter coordinates), 

      "etrs.laea" (for the ETRS-LAEA coordinates), 
 
      "xy" (not specified). }
  \item{grid.cors}{ A two element list, each beeing a vector with the 
      coordinates of the field in the x and y directions. The list should
      be named with the pertinent coordinate names for the coordinate
      system. If the list is not named and has two elements, the names 
      are added assuming the first/second element is the x/y coordinate . 
      This attribute is specifically for spatial 
      grids (i.e. matrices and arrays). It can be omitted for other data
      objects. }
  \item{grid.pars}{ A list with additional arguments needed for special 
      parameters of a grid type. The elements of the list depend on 
      the grid.type. 
      E.g. when \code{grid.type="rotpol"} the coordinates of the rotated pole
      are provided in list arguments \code{plon} and \code{plat}. For other
      coordinate systems no additional parameters are needed and this
      argument can be omitted. }
  \item{what}{ A character, specifying what kind of attributes (names)
      to return. 
      \code{what="all"} returns all grid attributes.
      \code{what="grid.type"} returns the grid.type attribute.
      \code{what="grid.cors"} returns the grid coordinate attributes.
      \code{what="grid.pars"} returns the grid parameter attributes.}
  \item{remove}{ Whether or not geographic attributes shall be deleted
      from the data object before adding new ones. }
  \item{from}{ The data object from which to copy the grid attributes. }
  \item{to}{ The data object to which grid attributes are copied to. }
  \item{g1}{ A data object with geographic attributes. }
  \item{g2}{ Also a data object with geographic attributes. }
}
\details{
  
}
\value{
  For \code{put.grid.atts}:

  The data object as specified in \code{fld} but with attributes added:

  E.g. if \code{grid.type="lonlat"} attributes \code{lon} and \code{lat} are
  added according to the input in argument \code{grid.cors}. 

  E.g. If \code{grid.type="rotpol"} attributes \code{rlon} and \code{rlat} are
  added according to the input in arguments \code{grid.cors} and the
  attributes \code{plon} and \code{plat} (location of rotated pole) 
  are added corresponding to the input
  in \code{grid.pars}.

  For \code{get.grid.atts}:

  A list with the pertinent grid attributes. If one or both coordinate
  attributes are missing in \code{fld}, only those existing are returned.
  Note that if 
  \code{what="grid.type"} the output is a list with one element 
  not a character string. 

  For \code{remove.grid.atts}:

  The data object \code{fld} but with all geographic attributes removed.  

  For \code{copy.grd.attr}:

  A field with the grid attributes added from another field.

  For \code{is.consistent.grid}:

  TRUE or FALSE depending on the consistency of grid-attributes.

}
\references{ }
\author{ Christoph Frei }
\note{ 
  It is possible that data objects are still not consistent even if 
  \code{is.consistent.grid} is TRUE. A test in full generality takes 
  long to program.
}
\seealso{ \code{\link{get.atts}}, \code{\link{put.atts}}, \code{\link{copy.atts}} }
\examples{

# Illustrations with a dummy object only
# --------------------------------------

hh <- "I am a dummy only."
hh <- put.grid.atts(hh,grid.type="lonlat",grid.cors=list(lon=c(2,3,4)))
attributes(hh)
hh <- remove.grid.atts(hh)
attributes(hh)
remove.grid.atts(hh)

hh <- "I am a dummy only."
hh <- put.grid.atts(hh,grid.type="rotpol",
                    grid.pars=list(plon=5,plat=5))
attributes(hh)

hh <- "I am a dummy only."
hh <- put.grid.atts(hh,grid.type="swisscors",grid.cors=list(c(2,3,4),c(4,3)))
attributes(hh)

get.grid.atts(hh,"grid.cors")
get.grid.atts(hh,"grid.type")
get.grid.atts(hh,"grid.pars")

hh1 <- "I am dummy nr 1."
hh1 <- put.grid.atts(hh1,grid.type="lonlat",grid.cors=list(lon=c(2,3,4)))
hh2 <- "I am dummy nr 2."
hh2 <- copy.grid.atts(from=hh1,to=hh2)
attributes(hh2)

hh1 <- "I am dummy nr 1."
hh1 <- put.grid.atts(hh1,grid.type="rotpol",
                     grid.cors=list(rlon=c(2,3,4)),
                     grid.pars=list(plon=2,plat=3))
hh2 <- "I am dummy nr 2."
hh2 <- put.grid.atts(hh2,grid.type="rotpol",
                     grid.cors=list(rlon=c(2,3,4)),
                     grid.pars=list(plon=1,plat=3))
is.consistent.grid(hh1,hh2)


\dontrun{
# Illustrations with a real data object
# -------------------------------------

require(gridmch)
data(prism.swiss02)
data(mask.swiss02.e)
get.grid.atts(prism.swiss02, what="all")

is.consistent.grid(prism.swiss02,mask.swiss02.e)
}
}
\keyword{ spatial }% at least one, from doc/KEYWORDS
