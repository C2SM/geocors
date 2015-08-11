`put.grid.atts` <-
function(fld,grid.type="xy",grid.cors=list(),grid.pars=list(),remove=TRUE) {

       # remove existing grid attributes first (if requested)
       if (remove) { fld <- remove.grid.atts(fld) }

       # check if input for grid.type is a regular grid.type
       res <- try(grid.att.names(grid.type),silent=TRUE)
       if ( "try-error" %in% class(res) ) {
          stop("Not a regular grid type: ",grid.type)
       }
       
       # deal with grid cors
       cor.nams <- grid.att.names(grid.type,what="grid.cors")
       if (is.null(grid.cors)) { grid.cors <- list() }
       if (!("list" %in% class(grid.cors))) {
          stop("grid.cors must be a list.")
       }
       if (length(grid.cors) != 0) {         # is grid.cors == list()
       	  # add names if they are missing but number of elements is ok
       	  if (is.null(names(grid.cors))) {
             if (length(grid.cors) == length(cor.nams)) {
                names(grid.cors) <- cor.nams
             } else {
             	stop("grid.cors must be a NAMED list.")
             }
       	  }
          # check for invalid names in grid.cors
          invalid <- !(names(grid.cors) %in% cor.nams)
          if (any(invalid)) { 
             stop("Invalid names in grid.cors: ",
                  paste(names(grid.cors)[invalid],collapse=", "))
          }
       }
       
       # deal with grid pars
       if (is.null(grid.pars)) { grid.pars <- list() }
       if (!("list" %in% class(grid.pars))) {
          stop("grid.pars must be a list.")
       }
       par.nams <- grid.att.names(grid.type,what="grid.pars")
       if (length(par.nams) == 0) {
       	  if (length(grid.pars) > 0) { 
       	     warning("grid.pars disregarded although provided.") }
          grid.pars <- list()
       }
       if ((length(grid.pars) == 0) & (length(par.nams) > 0)) {
          stop("Input for grid.pars needed: ", 
               paste(par.nams,collapse=", "))
       }
       if (length(grid.pars) != 0) {         # is grid.pars == list()
       	  # check if names and length of grid.pars are ok
       	  if (is.null(names(grid.pars))) {
             stop("grid.pars must be a NAMED list.")
          }
          if (length(grid.pars) != length(par.nams)) {
             stop("For grid.type ",grid.type,
                  " grid.pars must be a list of length ",length(par.nams), 
                  " with elements: ", paste(par.nams,collapse=", "))
          }
          invalid <- !(names(grid.pars) %in% par.nams)
          if (any(invalid)) { 
             stop("Invalid names in grid.pars: ",
                  paste(names(grid.pars)[invalid],collapse=", "))
          }
       }
       
       # finally put the attributes
       put.atts(fld,atts=c(list(grid.type=grid.type),
                           grid.cors,grid.pars))
}
