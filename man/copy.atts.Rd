\name{copy.atts}
\alias{copy.atts}
\alias{put.atts}
\alias{get.atts}
\title{ Get/Set/Copy user-defined attributes from/to/between objects}
\description{
  \code{get.atts} reads some/all attributes from a data object. 
  \code{put.atts} adds/sets attributes of an object to certain values. 
  \code{copy.atts} copies attributes from one object to another object. 
  These functions are made to deal with user-defined attributes.
}
\usage{
get.atts(from, which = NULL, 
         which.not = c("dim", "dimnames", "names", "row.names", "class"))

put.atts(to, atts = list(), 
         which.not = c("dim", "dimnames", "names", "row.names", "class"))

copy.atts(from, to, which = NULL, 
          which.not = c("dim", "dimnames", "names", "row.names", "class"))
}
\arguments{
  \item{from}{ The object from which attributes shall be read. }
  \item{to}{ The object to which attributes shall be added. }
  \item{which}{ A character vector with attribute names that shall
       be read / copies. If \code{NULL} (default) read/copy all 
       available attributes in \code{from} which are not contained
       in argument \code{which.not}. }
  \item{which.not}{ A character vector with attributes that shall
       not be touched. The default includes common internal attributes
       insuring that only user-defined attributes are handled. }
  \item{atts}{ A list with attributes (names and values) that shall
       be added to / modified in \code{to}. Typically this is the
       output from \code{get.atts}. }
}
\details{
  Handle with care and ensure that internal attributes are protected
  by using appropriate \code{which.not}.
}
\value{
  \code{put.atts()} and \code{copy.atts()} return object \code{to} with
     the modified attributes.

  \code{get.atts()} returns a named list with the requested attributes
     from \code{from}.
}
\references{  }
\author{ Christoph Frei }
\note{

}
\seealso{ \code{\link{copy.grid.atts}}, \code{\link{attributes}}, \code{\link{attr}} }
\examples{
# Define example objects
a <- array(1:12,dim=c(3,4,1))
attr(a,"lon") <- c(12,13,14,15)
attr(a,"lat") <- c(45,46,47,48)
b <- array(1:60,dim=c(3,4,5))
dimnames(b)[[3]] <- c("b1","b2","b3","b4","b5")
c <- c(3,4,5)

# get and manipulate attributes
get.atts(a)
get.atts(b)
cc <- put.atts(c,atts=get.atts(a))
attributes(cc)
bb <- copy.atts(a,b)
attributes(bb)

}
\keyword{ manip }
\keyword{ attribute }
