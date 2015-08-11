`is.consistent.grid` <-
function(g1,g2) {

       rel.tolerance <- 0.001

       g1.atts <- get.grid.atts(g1,what="all")
       g2.atts <- get.grid.atts(g2,what="all")

       # check consistency of grid types
       if (g1.atts$grid.type != g2.atts$grid.type) { return(FALSE) }
       g.t <- g1.atts$grid.type

       # check consistency of grid.parameters mildly
       g1.pars <- get.grid.atts(g1,what="grid.pars")
       g2.pars <- get.grid.atts(g2,what="grid.pars")
       if ((length(g1.pars) != 0) | (length(g2.pars) != 0)) {
          if (length(g1.pars) != length(g2.pars)) { return(FALSE) }
          if (any(!(names(g1.pars) %in% names(g2.pars)))) { return(FALSE) }
       }
       
       # special case for rotated pole coordinates
       if (g.t == "rotpol") {
          if ((g1.atts$plon - g2.atts$plon) > rel.tolerance*1.0) {return(FALSE)}
          if ((g1.atts$plat - g2.atts$plat) > rel.tolerance*1.0) {return(FALSE)}
       }

       # check coordinates
       cornams <- grid.att.names(g.t,what="grid.cors")
       if ((cornams[1] %in% names(g1.atts)) & (cornams[1] %in% names(g2.atts))) {
         x1 <- g1.atts[[cornams[1]]]
         x2 <- g2.atts[[cornams[1]]]
         dx <- diff(x1)[1]
         nx <- length(x1)
         if (length(x1) != length(x2)) { return(FALSE) }
         if ((x1[1]-x2[1]) > rel.tolerance*dx) { return(FALSE) }
         if ((x1[nx]-x2[nx]) > rel.tolerance*dx) { return(FALSE) }
         rm(x1,x2,dx,nx)
       }
       if ((cornams[2] %in% names(g1.atts)) & (cornams[2] %in% names(g2.atts))) {
         y1 <- g1.atts[[cornams[2]]]
         y2 <- g2.atts[[cornams[2]]]
         dy <- diff(y1)[1]
         ny <- length(y1)
         if (length(y1) != length(y2)) { return(FALSE) }
         if ((y1[1]-y2[1]) > rel.tolerance*dy) { return(FALSE) }
         if ((y1[ny]-y2[ny]) > rel.tolerance*dy) { return(FALSE) }
         rm(y1,y2,dy,ny)
       }

       TRUE
}
