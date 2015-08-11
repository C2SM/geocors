`get.grid.atts` <-
function(fld,what="all") {

       grid.type <- attr(fld,"grid.type")

       if (is.null(grid.type)) {
          stop("No geographic attributes specified in fld.") 
       }

       cor.nams <- grid.att.names(grid.type,what="grid.cors")
       par.nams <- grid.att.names(grid.type,what="grid.pars")
       type.nam <- "grid.type"

       # select only those coordinate attributes that are available
       cor.atts <- get.atts(fld,which=cor.nams)
       names(cor.atts) <- cor.nams
       not.av <- unlist(lapply(cor.atts,FUN=is.null))
       cor.atts <- cor.atts[!not.av]

       # get parameter attributes and check if all are available
       if (is.null(par.nams)) { par.atts <- list() }
       else {
          par.atts <- get.atts(fld,which=par.nams)
          names(par.atts) <- par.nams
          not.av <- unlist(lapply(par.atts,FUN=is.null))
          if (any(not.av)) {
             stop("Some geographic attributes are missing in fld : ",
                  paste(par.nams[not.av],collapse=", "))
          }
       }

       switch(what,
           "all" = c(list(grid.type=grid.type),cor.atts,par.atts),
           "grid.type" = list(grid.type=grid.type),
           "grid.cors" = cor.atts,
           "grid.pars" = par.atts)

}



