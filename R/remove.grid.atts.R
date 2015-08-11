`remove.grid.atts` <-
function(fld) {

   grid.type <- attr(fld,"grid.type")
   if (is.null(grid.type)) { return(fld) }

   att.nams <- grid.att.names(grid.type,what="all")

   for (nn in att.nams) { attr(fld,nn) <- NULL }

   fld
}

