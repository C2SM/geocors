`copy.grid.atts` <-
function(from,to) {

       gt <- get.grid.atts(from,what="grid.type")
       gc <- get.grid.atts(from,what="grid.cors")
       gp <- get.grid.atts(from,what="grid.pars")

       put.grid.atts(to,grid.type=gt[[1]],grid.cors=gc,grid.pars=gp)

}



