`lonlat2rotpol` <-
function (lon,lat,plon,plat) {
#  converts normal lat-lon coordinates into coordinates with a rotated pole.
#  returns a matrix with two columns (rlon, rlat)
#  implemented after fortran routines in EM/HM (Majewski, Bresch)

   # parameter renaming
   polphi <- plat; pollam <- plon

   # allow for xy.coord input
   if (class(lon) == "list") { 
      a <- lon[[1]]; b <- lon[[2]]
      lon <- a; lat <- b
      rm(a,b)
   }

   # allow for matrix input
   if (class(lon) == "matrix") { 
      if (dim(lon)[2] != 2) {
        stop("Matrix lon does not have two columns.")
      }
      a <- lon[,1]; b <- lon[,2]
      lon <- a; lat <- b 
      rm(a,b)
   }

   #  case without rotation
   if (abs(polphi-90.0)<0.001) {
     return(matrix(c(lon,lat),ncol=2))
   }

   # initialize working variables
   zrpi18 <- 360./(2*pi)
   zpir18 <- 2*pi/360
   zsinpol <- sin(zpir18*polphi)
   zcospol <- cos(zpir18*polphi)
   zlampol <- zpir18*pollam

   # preparation
   zlat <- zpir18*lat
   zlon <- lon
   ii <- (zlon>180)
   zlon[ii] <- zlon[ii] - 360.0
   zlon <- zpir18*zlon

   # first, the conversion of lat to rlat:
   aarg <- zcospol*cos(zlat)*cos(zlon-zlampol) + zsinpol*sin(zlat)
   rlat <- zrpi18*asin(aarg)

   # then, conversion of lon to rlon:
   zarg1 <- -sin(zlon-zlampol)*cos(zlat)
   zarg2 <- -zsinpol*cos(zlat)*cos(zlon-zlampol) + 
             zcospol*sin(zlat)
   ii1 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) < 1e-30)) 
   ii2 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 > 0))
   ii3 <- ((abs(zarg2) < 1e-30) & (abs(zarg1) >= 1e-30) & (zarg1 <= 0))
   ii4 <- (abs(zarg2) >= 1e-30)
   rlon <- zlon
   rlon[ii1] <- 0.0   
   rlon[ii2] <- 90.0   
   rlon[ii3] <- -90.0
   rlon[ii4] <- zrpi18*atan(zarg1/zarg2)

   # return matrix
   matrix(c(rlon,rlat),ncol=2)
}

