`grid.att.names` <-
function(grid.type,what="all") {

       coord.nams <- switch(grid.type,
         "lonlat"       = c("lon","lat"),
         "rotpol"       = c("rlon","rlat"),
         "swisscors"    = c("chx","chy"),
         "xy"           = c("x","y"),
         "etrs.laea"    = c("X","Y"),
         "etrs.austria" = c("X","Y"),
         "etrs.utm32"   = c("X","Y"),
         stop("grid type: ", grid.type, " not implemented."))

       par.nams <- switch(grid.type,
         "lonlat"       = c(),
         "rotpol"       = c("plon","plat"),
         "swisscors"    = c(),
         "xy"           = c(),
         "etrs.laea"    = c(),
         "etrs.austria" = c(),
         "etrs.utm32"   = c(),
         stop("grid type: ", grid.type, " not implemented."))

       type.nam <- "grid.type"

       switch(what,
          "all" = c(type.nam,coord.nams,par.nams),
          "grid.type" = type.nam,
          "grid.cors" = coord.nams,
          "grid.pars" = par.nams,
          stop("Invalid value for argument what: ",what)) 
       
}
