`geocors.trafo` <-
function(x,y=NULL,from.type,from.pars,to.type,to.pars) {
	
    # checks
    cl.in <- class(x)
    if (!(cl.in %in% c("numeric","matrix","data.frame","array","list"))) {
       stop("x must be a numeric vector, matrix, data frame, list or array")
    }
    
    # check / complete input specifications
    # -------------------------------------------------------------------------
    
    # find coordinate system type for input if not specified
    if (missing(from.type)) {
       gatts <- try(get.grid.atts(x,what="grid.type"),silent=TRUE)
       if (!("try-error" %in% class(gatts))) {
          from.type <- gatts$grid.type
       } else {
          stop("from.type not specified")
       }
       rm(gatts)
    }

    # define coordinate names for input grid
    from.cor.nams <- grid.att.names(from.type,what="grid.cors")

    # define parameter names for input grid
    from.par.nams <- grid.att.names(from.type,what="grid.pars")
    
    # find input grid.pars if necessary
    if (!is.null(from.par.nams)) {
       if (!missing(from.pars)) {
          # a) try first from input to from.pars
          if (!("list" %in% class(from.pars))) {
             stop("from.pars should be a list.")
          }
          av <- (from.par.nams %in% names(from.pars))
          if ( !all(av) ) {
             stop("input for parameter(s) ", from.par.nams[!av], " missing in from.pars.")          
          } else {
       	     from.pars <- from.pars[from.par.nams]
          }
          rm(av)
       } else {
          # b) try to get parameters from object attributes
          gatts <- try(get.grid.atts(x,what="grid.pars"),silent=TRUE)
          if ("try-error" %in% class(gatts)) {
             stop("from.pars not specified")
          } 
          from.pars <- gatts       
          av <- (from.par.nams %in% names(from.pars))
          if ( !all(av) ) {
             stop("input for parameter(s) ", from.par.nams[!av], " missing in attributes.")          
          } else {
       	     from.pars <- from.pars[from.par.nams]
          }
          rm(av)
       }
    } else { from.pars <- NULL }

    # make sure to.type is specified    
    if (missing(to.type)) {
       stop("to.type not specified")
    }
    
    # define coordinate names for output grid
    to.cor.nams <- grid.att.names(to.type,what="grid.cors")

    # make sure to.pars are specified if necessary
    to.par.nams <- grid.att.names(to.type,what="grid.pars")
    if (!is.null(to.par.nams)) {
       if (missing(to.pars)) {
          stop("need input for to.pars")
       }
       if (!("list" %in% class(to.pars))) { 
         stop("to.pars should be a list.")
       }
       av <- (to.par.nams %in% names(to.pars))
       if ( !all(av) ) {
          stop("input for parameter(s) ", to.par.nams[!av], " missing in to.pars.")
       }
       rm(av)
    } else { to.pars <- NULL }
    
    
    # convert input data into two column matrix (with named columns) --> xy
    # -------------------------------------------------------------------------
    
    # case where x/y are provided individually
    if ("numeric" %in% class(x)) {
       inp.x <- "numeric"
       row.nams <- NULL
       if (missing(y)) { stop("missing input for y") }
       if (is.null(y)) { stop("missing input for y") }
       if (length(x) != length(y)) { stop("unequal length of x and y")}
       xy <- cbind(x,y)
       colnames(xy) <- from.cor.nams
    }
    # case where x/y are provided as columns of a matrix
    if ("matrix" %in% class(x)) {
       inp.x <- "matrix"
       row.nams <- rownames(x)
       if (dim(x)[2] != 2) {
          stop("input matrix x does not have two columns")
       }
       if (is.null(colnames(x))) {
          colnames(x) <- from.cor.nams
       }
       xy <- x[,from.cor.nams]
    }    
    # case where x/y are provided as columns of a data.frame
    if ("data.frame" %in% class(x)) {
       inp.x <- "data.frame"
       row.nams <- rownames(x)
       if (dim(x)[2] != 2) {
          stop("input data.frame x does not have two columns")
       }
       if (is.null(names(x))) {
          names(x) <- from.cor.nams
       }
       if (!all(from.cor.nams %in% names(x))) {
          stop("inappropriate coordinate names in data.frame x")
       }
       xy <- as.matrix(x[,from.cor.nams])
    }    
    # case where x/y are provided as elements of a list
    if ("list" %in% class(x)) {
       inp.x <- "list"
       row.nams <- NULL
       if (length(x) != 2) {
          stop("input list x does not have two elements")
       }
       if (length(x[[1]]) != length(x[[2]])) {
          stop("list elements in x need to be of similar length")
       }
       if (is.null(names(x))) {
          names(x) <- from.cor.nams
       }
       if (!all(from.cor.nams %in% names(x))) {
          stop("inappropriate coordinate names in list x")
       }
       xy <- cbind(x[[from.cor.nams[1]]],x[[from.cor.nams[2]]])
       colnames(xy) <- from.cor.nams
    }    
    # case where x/y are provided as levels of a 3D array
    if ("array" %in% class(x)) {
       inp.x <- "array"
       dimx <- dim(x)
       row.nams <- NULL
       if (length(dimx) != 3) { stop("input array x does not have three dimensions") }
       if (dim(x)[3] != 2) { stop("input array x does not have two levels") }
       if (is.null(dimnames(x)[[3]])) {
          dimnames(x)[[3]] <- from.cor.nams
       } 
       if (!all(from.cor.nams %in% dimnames(x)[[3]])) {
          stop("inappropriate coordinate names (dimnames[[3]]) in array x")
       }
       xy <- cbind(as.vector(x[,,from.cor.nams[1]]),
                   as.vector(x[,,from.cor.nams[2]]))
       colnames(xy) <- from.cor.nams
    }    


    # do coordinate transformation
    # -------------------------------------------------------------------------
    trafo.nam <- paste("trafo.",from.type,"2",to.type,sep="")
    if (!exists(trafo.nam)) {
       stop("Coordinate transformation ",trafo.nam," not implemented.")
    }
    xyt <- do.call(trafo.nam,args=list(xy=xy,from.pars=from.pars,to.pars=to.pars))
    
        
    # rebuild output data objects
    # -------------------------------------------------------------------------
    
    # case where x/y are provided individually or as a two element list
    if ((inp.x == "numeric") | (inp.x == "list")) { 
       xyt <- list(xyt[,1],xyt[,2])
       names(xyt) <- to.cor.nams
    }
    # case where x/y are provided as columns of a matrix
    if (inp.x == "matrix") { 
       xyt <- as.matrix(xyt[,c(1,2)])
       colnames(xyt) <- to.cor.nams
       if (!is.null(row.nams)) { rownames(xyt) <- row.nams }
    }
    # case where x/y are provided as data.frame
    if (inp.x == "data.frame") { 
       if (dim(xyt)[1] == 1) { 
          xyt <- as.data.frame(xyt)
       } else {
          xyt <- as.data.frame(xyt[,c(1,2)])
       }
       names(xyt) <- to.cor.nams
       if (!is.null(row.nams)) { rownames(xyt) <- row.nams }
    }
    # case where x/y are provided as levels of a 3D array
    if (inp.x == "array") { 
       xyt <- array(xyt[,c(1,2)],dim=dimx)
       dimnames(xyt)[[3]] <- to.cor.nams
    }
    
    # add attributes
    # -------------------------------------------------------------------------
    if (!is.null(to.pars)) {
       xyt <- put.atts(to=xyt,atts=to.pars)
    }
    attr(xyt,"grid.type") <- to.type
    
    
    # return result
    # -------------------------------------------------------------------------
    xyt
    
}
